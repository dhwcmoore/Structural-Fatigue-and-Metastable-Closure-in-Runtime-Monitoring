(* Rupture Certificate builder and JSON serialiser — Tier 3 artifact.
   The certificate is the "fossilized trace" of a failed concrescence:
   it explains WHY a safe distinction could not be maintained, not merely
   THAT it failed. *)

open Types

(* ── Phenomenological residue construction ───────────────────────────── *)

let build_residue (state : machine_state) (final_dist : float)
    : phenomenological_residue =
  let snaps = state.snapshots in
  (* Curvature divergence: rate of change relative to the remaining distance *)
  let curvature =
    abs_float state.prev_gradient /. (max 0.001 state.prev_dist)
  in
  let unresolved = List.length snaps in
  let rupture_pressure = state.tension.tension_score in
  let summary =
    if unresolved = 0 then
      Printf.sprintf
        "Abrupt collapse at t=%d with no prior META_REVIEW. \
         Projection distance fell to %.4f without warning."
        state.timestep final_dist
    else begin
      let last_snap = List.hd snaps in
      Printf.sprintf
        "Unresolved bifurcation beginning at t=%d (%s) led to forced \
         collapse of pressure-sensitivity boundary after %d revision(s). \
         Final projection distance: %.4f."
        last_snap.timestamp
        last_snap.reason
        state.tension.boundary_revisions
        final_dist
    end
  in
  { curvature_divergence         = curvature;
    interval_widening_frequency  = state.tension.interval_widen_count;
    unresolved_bifurcation_count = unresolved;
    rupture_pressure;
    inheritance_summary          = summary;
  }

(* ── Certificate constructor ─────────────────────────────────────────── *)

let build (state : machine_state)
          (mx : float array) (my : float array)
          (x  : float array) (y  : float array)
          (ax : action)      (ay : action)
          (final_dist : float) : rupture_certificate =
  let witness = {
    input_x      = x;  input_y      = y;
    projection_x = mx; projection_y = my;
    action_x     = ax; action_y     = ay;
  } in
  let residue = build_residue state final_dist in
  (* The "latent projection" is the collapsed midpoint — the single
     indistinguishable point in action space. *)
  let latent =
    Array.init (Array.length mx) (fun i -> (mx.(i) +. my.(i)) /. 2.0)
  in
  { cert_status              = "HARD_RUPTURE";
    latent_projection        = latent;
    witness;
    phenomenological_residue = residue;
    meta_state_status        = "failed_stabilization";
  }

(* ── JSON serialiser ─────────────────────────────────────────────────── *)

let fmt_arr a =
  String.concat ", " (Array.to_list (Array.map (Printf.sprintf "%.4f") a))

let to_json (cert : rupture_certificate) : string =
  let w = cert.witness in
  let r = cert.phenomenological_residue in
  Printf.sprintf {|{
  "status": "%s",
  "latent_projection": [%s],
  "witness_pair": {
    "logic": "admissibility_violation",
    "input_x_dim0": %.4f,
    "input_y_dim0": %.4f,
    "projection_x": [%s],
    "projection_y": [%s],
    "action_x": "%s",
    "action_y": "%s",
    "divergence": "safety_critical"
  },
  "phenomenological_residue": {
    "curvature_divergence": %.4f,
    "interval_widening_frequency": %d,
    "unresolved_bifurcation_count": %d,
    "rupture_pressure": %.4f,
    "inheritance_summary": "%s"
  },
  "meta_state_status": "%s"
}|}
    cert.cert_status
    (fmt_arr cert.latent_projection)
    w.input_x.(0)
    w.input_y.(0)
    (fmt_arr w.projection_x)
    (fmt_arr w.projection_y)
    (pp_action w.action_x)
    (pp_action w.action_y)
    r.curvature_divergence
    r.interval_widening_frequency
    r.unresolved_bifurcation_count
    r.rupture_pressure
    r.inheritance_summary
    cert.meta_state_status
