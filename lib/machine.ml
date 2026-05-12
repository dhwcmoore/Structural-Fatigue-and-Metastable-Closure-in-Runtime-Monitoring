(* STAK-PSAL state machine.
   Implements the three-status cycle:
     CLOSED → META_REVIEW → HARD_RUPTURE
   and the three-tier data pipeline (Rolling / Snapshot / Certificate). *)

open Types
open Admissibility

let max_snapshots = 8

(* ── Initialisation ──────────────────────────────────────────────────── *)

let make_initial (proj_fn : float array -> float array)
                 (x : float array) (y : float array) : machine_state =
  let mx = proj_fn x and my = proj_fn y in
  let d  = Vec.dist mx my in
  (* Seed separation_ema and rupture_pressure from the actual initial distance
     so the EMA does not start from 0 and produce a misleading transient. *)
  let t0 = { Tension.empty with
    separation_ema   = d;
    rupture_pressure = 1.0 /. (d +. 0.001);
  } in
  { status        = Closed;
    projection_x  = mx;
    projection_y  = my;
    tension       = t0;
    timestep      = 0;
    prev_dist     = d;
    prev_gradient = 0.0;
    snapshots     = [];
    tension_at_enrichment = 0.0;
    steps_in_meta         = 0;
  }

(* ── Snapshot helpers ────────────────────────────────────────────────── *)

let recent_deltas_from (state : machine_state) (d : float) : float list =
  let prev =
    match state.status with
    | MetaReview snap -> snap.recent_deltas
    | _              -> []
  in
  d :: List.filteri (fun i _ -> i < 4) prev

let push_snapshot (state : machine_state) (snap : audit_snapshot)
    : audit_snapshot list =
  let snaps = snap :: state.snapshots in
  List.filteri (fun i _ -> i < max_snapshots) snaps

(* ── Single recurrent step ───────────────────────────────────────────── *)

(* step state w x y
   Returns the next machine_state given the current projection matrix W
   and the two fixed input vectors (x = Normal Pressure, y = Critical Surge).
   W is passed in from the environment; context enrichment is signalled
   via the returned [enrichment] option so the environment can apply it. *)

type step_result = {
  next_state : machine_state;
  enrichment : (float array array) option;   (* Some w' if context enrichment was applied *)
}

let step ?(boost=0.25)
         (state : machine_state)
         (w     : float array array)
         (x     : float array)
         (y     : float array) : step_result =
  let proj_fn = Lnp.project w in

  match check proj_fn x y with

  (* ── HARD_RUPTURE: admissibility has been violated ─────────────── *)
  | HardRupture_ (mx, my, ax, ay, d) ->
    let grad  = d -. state.prev_dist in
    let new_t = Tension.update state.tension d grad state.prev_gradient in
    let cert  = Certificate.build state mx my x y ax ay d in
    { next_state =
        { state with
          status        = HardRupture cert;
          projection_x  = mx;  projection_y  = my;
          tension       = new_t;
          timestep      = state.timestep + 1;
          prev_dist     = d;   prev_gradient = grad;
        };
      enrichment = None;
    }

  (* ── META_REVIEW: projections drifting into collapse zone ──────── *)
  | Drifting_ (mx, my, d) ->
    let grad  = d -. state.prev_dist in
    let new_t =
      Tension.update state.tension d grad state.prev_gradient
      |> Tension.on_meta_review
    in
    let reason =
      if   new_t.oscillation_count > 3    then "OSCILLATION"
      else if d < epsilon_collapse +. 0.04 then "PROXIMITY_ALERT"
      else "OOD_SIGNAL"
    in
    let snap = {
      reason;
      boundary_snapshot = Array.copy w.(0);
      recent_deltas     = recent_deltas_from state d;
      tension_at_entry  = new_t;
      timestamp         = state.timestep;
    } in
    let new_t1 = Tension.on_boundary_revision new_t in
    let w'     = Lnp.context_enrich w x y boost in
    (* Recovery elasticity: E_r = (tension_at_entry - current) / steps_since_entry.
       Positive = enrichment reducing strain; negative = strain still rising. *)
    let is_first_meta = match state.status with Closed -> true | _ -> false in
    let new_tae =
      if is_first_meta then new_t1.tension_score
      else state.tension_at_enrichment
    in
    let new_sim =
      if is_first_meta then 1
      else state.steps_in_meta + 1
    in
    let er =
      (new_tae -. new_t1.tension_score) /. float_of_int (max 1 new_sim)
    in
    let new_t2 = { new_t1 with recovery_elasticity = er } in
    { next_state =
        { status        = MetaReview snap;
          projection_x  = mx;  projection_y  = my;
          tension       = new_t2;
          timestep      = state.timestep + 1;
          prev_dist     = d;   prev_gradient = grad;
          snapshots     = push_snapshot state snap;
          tension_at_enrichment = new_tae;
          steps_in_meta         = new_sim;
        };
      enrichment = Some w';
    }

  (* ── CLOSED: boundary discipline intact ────────────────────────── *)
  | Admissible_ (mx, my, d) ->
    let grad  = d -. state.prev_dist in
    let new_t = Tension.update state.tension d grad state.prev_gradient in
    { next_state =
        { state with
          status        = Closed;
          projection_x  = mx;  projection_y  = my;
          tension       = new_t;
          timestep      = state.timestep + 1;
          prev_dist     = d;   prev_gradient = grad;
          steps_in_meta = 0;   (* reset episode counter on reclosure *)
        };
      enrichment = None;
    }
