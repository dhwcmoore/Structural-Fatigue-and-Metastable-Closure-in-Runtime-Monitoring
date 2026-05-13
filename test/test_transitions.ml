(* Regression tests for STAK-PSAL state machine transitions.
   Each test verifies one invariant.  Exit 0 = all pass. *)

open Admissibility
open Types

(* ── Helpers ─────────────────────────────────────────────────────────── *)

let pass label =
  Printf.printf "  PASS  %s\n%!" label

let fail label msg =
  Printf.printf "  FAIL  %s — %s\n%!" label msg;
  exit 1

(* Noise-free 64D sensor vectors: dim 0 differs, all others = 0.50 *)
let x_normal   = Array.init 64 (fun i -> if i = 0 then 0.20 else 0.50)
let y_critical = Array.init 64 (fun i -> if i = 0 then 0.90 else 0.50)

(* Build a clean 2×64 W with a given weight on dim 0 only (row 1 is also
   deterministic so row-1 distance stays zero for noise-free inputs). *)
let make_w w00 =
  let w = Array.init 2 (fun _ -> Array.make 64 0.0) in
  w.(0).(0) <- w00;
  w.(1).(1) <- 1.0;  (* fixed row-1 weight so M(x)[1] = M(y)[1] always *)
  w.(1).(2) <- 1.0;
  w

(* ── Test 1: Safety function Φ partitions the inputs correctly ────────── *)

let () =
  let label = "phi(x_normal) = Maintain" in
  if phi x_normal = Maintain then pass label
  else fail label "expected Maintain"

let () =
  let label = "phi(y_critical) = EStop" in
  if phi y_critical = EStop then pass label
  else fail label "expected EStop"

(* ── Test 2: Admissible_ when dist > ε_meta ─────────────────────────── *)

let () =
  let label = "Admissible_ returned when dist > ε_meta" in
  (* W[0][0] = 5.0: dist ≈ 0.226, well above ε_meta = 0.22 *)
  let w = make_w 5.0 in
  match check (Lnp.project w) x_normal y_critical with
  | Admissible_ (_, _, d) ->
    if d > epsilon_meta then pass label
    else fail label (Printf.sprintf "dist %.4f not > ε_meta %.4f" d epsilon_meta)
  | Drifting_    _ -> fail label "got Drifting_"
  | HardRupture_ _ -> fail label "got HardRupture_"

(* ── Test 3: Drifting_ when ε_collapse < dist < ε_meta ────────────��─── *)

let () =
  let label = "Drifting_ returned when ε_collapse < dist < ε_meta" in
  (* W[0][0] = 1.0: dist = |σ(0.9) − σ(0.2)| ≈ 0.161, in (0.08, 0.22) *)
  let w = make_w 1.0 in
  match check (Lnp.project w) x_normal y_critical with
  | Drifting_ (_, _, d) ->
    if d > epsilon_collapse && d < epsilon_meta then pass label
    else fail label (Printf.sprintf "dist %.4f not in (%.4f, %.4f)" d epsilon_collapse epsilon_meta)
  | Admissible_ _ -> fail label "got Admissible_"
  | HardRupture_ _ -> fail label "got HardRupture_"

(* ── Test 4: HardRupture_ when dist < ε_collapse and actions diverge ─── *)

let () =
  let label = "HardRupture_ returned when dist < ε_collapse" in
  (* W[0][0] = 0.1: dist = |σ(0.09) − σ(0.02)| ≈ 0.017 < 0.08 *)
  let w = make_w 0.1 in
  match check (Lnp.project w) x_normal y_critical with
  | HardRupture_ (_, _, ax, ay, d) ->
    if d < epsilon_collapse then
      if ax = Maintain && ay = EStop then pass label
      else fail label "witness actions wrong"
    else fail label (Printf.sprintf "dist %.4f not < ε_collapse %.4f" d epsilon_collapse)
  | Admissible_ _ -> fail label "got Admissible_"
  | Drifting_    _ -> fail label "got Drifting_"

(* ── Test 5: HardRupture_ requires divergent actions ────────────────── *)
(* If both inputs map to the same action, collapsing projections is safe. *)

let () =
  let label = "No HardRupture_ when actions are identical (both Maintain)" in
  let x2 = Array.init 64 (fun i -> if i = 0 then 0.20 else 0.50) in
  let y2 = Array.init 64 (fun i -> if i = 0 then 0.30 else 0.50) in
  (* Both x2[0]=0.20 and y2[0]=0.30 are below 0.70 → both Maintain *)
  let w = make_w 0.1 in
  (match check (Lnp.project w) x2 y2 with
   | HardRupture_ _ -> fail label "should not rupture when Φ(x) = Φ(y)"
   | Drifting_ _ | Admissible_ _ -> pass label)

(* ── Test 6: Context enrichment increases W[0][0] ────────────────────── *)

let () =
  let label = "context_enrich boosts the most-discriminative weight" in
  let w  = make_w 1.0 in
  let w' = Lnp.context_enrich w x_normal y_critical 0.5 in
  if w'.(0).(0) > w.(0).(0) then pass label
  else fail label (Printf.sprintf "W[0][0] did not increase: was %.4f, got %.4f"
                    w.(0).(0) w'.(0).(0))

(* ── Test 7: Degradation reduces W[0][0] ────────────────────────────── *)

let () =
  Random.init 0;
  let label = "degrade reduces W[0][0]" in
  let w  = make_w 3.0 in
  let w' = Lnp.degrade w 0.05 in
  if w'.(0).(0) < w.(0).(0) then pass label
  else fail label (Printf.sprintf "W[0][0] did not decrease: was %.4f, got %.4f"
                    w.(0).(0) w'.(0).(0))

(* ── Test 8: Machine returns CLOSED when Admissible_ ────────────────── *)

let () =
  Random.init 0;
  let label = "Machine.step → Closed when dist > ε_meta" in
  let w  = make_w 5.0 in
  let s0 = Machine.make_initial (Lnp.project w) x_normal y_critical in
  let result = Machine.step s0 w x_normal y_critical in
  (match result.next_state.status with
   | Closed -> pass label
   | MetaReview _ -> fail label "got MetaReview"
   | HardRupture _ -> fail label "got HardRupture")

(* ── Test 9: Machine returns MetaReview when Drifting_ ──────────────── *)

let () =
  Random.init 0;
  let label = "Machine.step → MetaReview when ε_collapse < dist < ε_meta" in
  let w  = make_w 1.0 in
  let s0 = Machine.make_initial (Lnp.project w) x_normal y_critical in
  let result = Machine.step s0 w x_normal y_critical in
  (match result.next_state.status with
   | MetaReview _ -> pass label
   | Closed       -> fail label "got Closed"
   | HardRupture _ -> fail label "got HardRupture")

(* ── Test 10: Machine returns HardRupture when collapse confirmed ──── *)

let () =
  Random.init 0;
  let label = "Machine.step → HardRupture when dist < ε_collapse and Φ diverges" in
  let w  = make_w 0.1 in
  let s0 = Machine.make_initial (Lnp.project w) x_normal y_critical in
  let result = Machine.step s0 w x_normal y_critical in
  (match result.next_state.status with
   | HardRupture _ -> pass label
   | Closed        -> fail label "got Closed"
   | MetaReview _  -> fail label "got MetaReview")

(* ── Test 11: Enrichment fires only on CLOSED → META_REVIEW ─────────── *)

let () =
  Random.init 0;
  let label = "MetaReview step returns Some enrichment" in
  let w  = make_w 1.0 in
  let s0 = Machine.make_initial (Lnp.project w) x_normal y_critical in
  let result = Machine.step s0 w x_normal y_critical in
  (match result.next_state.status, result.enrichment with
   | MetaReview _, Some _ -> pass label
   | MetaReview _, None   -> fail label "expected enrichment option"
   | _ -> fail label "unexpected status")

(* ── Test 12: Tension score is non-negative ──────────────────────────── *)

let () =
  Random.init 0;
  let label = "Tension score is non-negative after any step" in
  let w  = make_w 1.0 in
  let s0 = Machine.make_initial (Lnp.project w) x_normal y_critical in
  let result = Machine.step s0 w x_normal y_critical in
  let t = result.next_state.tension.tension_score in
  if t >= 0.0 then pass label
  else fail label (Printf.sprintf "tension = %.6f" t)

(* ── Test 13: Rupture certificate has correct witness actions ─────────── *)

let () =
  Random.init 0;
  let label = "Rupture certificate records correct divergent actions" in
  let w  = make_w 0.1 in
  let s0 = Machine.make_initial (Lnp.project w) x_normal y_critical in
  let result = Machine.step s0 w x_normal y_critical in
  (match result.next_state.status with
   | HardRupture cert ->
     let ax = cert.witness.action_x and ay = cert.witness.action_y in
     if ax = Maintain && ay = EStop then pass label
     else fail label (Printf.sprintf "actions: %s / %s" (pp_action ax) (pp_action ay))
   | _ -> fail label "no HardRupture")

(* ── Test 14: separation_ema and rupture_pressure are positive after a step ── *)

let () =
  Random.init 0;
  let label = "separation_ema and rupture_pressure positive after any step" in
  let w  = make_w 5.0 in
  let s0 = Machine.make_initial (Lnp.project w) x_normal y_critical in
  let s1 = (Machine.step s0 w x_normal y_critical).next_state in
  if s1.tension.separation_ema > 0.0 && s1.tension.rupture_pressure > 0.0
  then pass label
  else fail label (Printf.sprintf "sep_ema=%.4f  rupt_press=%.4f"
    s1.tension.separation_ema s1.tension.rupture_pressure)

(* ── Test 15: steps_in_meta and recovery_elasticity tracked in META_REVIEW ── *)

let () =
  Random.init 0;
  let label = "steps_in_meta increments and elasticity computed in META_REVIEW" in
  (* W[0][0] = 1.0 → Drifting_; tension accumulates so elasticity < 0 by step 2 *)
  let w  = make_w 1.0 in
  let s0 = Machine.make_initial (Lnp.project w) x_normal y_critical in
  let step s = (Machine.step s w x_normal y_critical).next_state in
  let s1 = step s0 in   (* first META_REVIEW step: steps_in_meta = 1 *)
  let s2 = step s1 in   (* second step: steps_in_meta = 2             *)
  let ok_steps = s1.steps_in_meta = 1 && s2.steps_in_meta = 2 in
  (* Tension accumulates in sustained META_REVIEW so elasticity ≤ 0 *)
  let ok_elast = s2.tension.recovery_elasticity <= 0.001 in
  if ok_steps && ok_elast then pass label
  else fail label (Printf.sprintf "steps=%d,%d  elast=%.6f"
    s1.steps_in_meta s2.steps_in_meta s2.tension.recovery_elasticity)

(* ── Test 16: Instability.estimate → Critical under near-collapse W ──────── *)

let () =
  Random.init 0;
  let label = "Instability.estimate = Critical for near-collapse projection" in
  (* W[0][0] = 0.52 → d ≈ 0.089 (just above ε_collapse); rupture_pressure ≈ 11 *)
  let w  = make_w 0.52 in
  let s0 = Machine.make_initial (Lnp.project w) x_normal y_critical in
  let step s = (Machine.step s w x_normal y_critical).next_state in
  let s4 = step (step (step (step s0))) in
  (* After 4 META_REVIEW steps: steps_in_meta=4, tension_slope >> 0.002,
     rupture_pressure >> 10, elasticity < 0 → Critical *)
  (match Instability.estimate s4 with
   | Instability.Critical -> pass label
   | Instability.Strained -> fail label "expected Critical, got Strained"
   | Instability.Stable   -> fail label "expected Critical, got Stable")

(* ── Test 17: horizon_estimate is large when convergence is near zero ─── *)

let () =
  Random.init 0;
  let label = "horizon_estimate is large when dist is stable (no convergence)" in
  (* W[0][0] = 1.0 → Drifting_; same W across steps so gradient ≈ 0. *)
  let w  = make_w 1.0 in
  let s0 = Machine.make_initial (Lnp.project w) x_normal y_critical in
  let s1 = (Machine.step s0 w x_normal y_critical).next_state in
  let h  = Instability.horizon_estimate s1 in
  (* No convergence → denom = horizon_eta → h ≈ (d - ε_c) / 1e-4 >> 100 *)
  if h > 100 then pass label
  else fail label (Printf.sprintf "expected h > 100, got h = %d" h)

(* ── Test 18: horizon_estimate is small under rapid convergence ───────── *)

let () =
  Random.init 0;
  let label = "horizon_estimate ≤ 2 under rapid convergence (forced-escalation zone)" in
  (* Use two W values to manufacture a steep negative gradient.
     Step 1: W[0][0] = 2.0 → larger d.
     Step 2: W[0][0] = 0.6 → smaller d (rapid drop).
     The resulting gradient reflects the sharp convergence.              *)
  let w_hi = make_w 2.0 in
  let w_lo = make_w 0.6 in
  let s0 = Machine.make_initial (Lnp.project w_hi) x_normal y_critical in
  let s1 = (Machine.step s0 w_hi x_normal y_critical).next_state in
  let s2 = (Machine.step s1 w_lo x_normal y_critical).next_state in
  let h  = Instability.horizon_estimate s2 in
  if h <= 2 then pass label
  else fail label (Printf.sprintf "expected h ≤ 2 under rapid convergence, got h = %d" h)

(* ── Test 19: forced escalation fires at steps_in_meta = 1 ──────────── *)

let () =
  Random.init 0;
  let label = "Instability.estimate = Critical at steps_in_meta = 1 under horizon debt" in
  (* Same two-W setup as test 18 but we verify the instability classification.
     s2 is in MetaReview with horizon ≤ 2 → horizon_debt = true → Critical. *)
  let w_hi = make_w 2.0 in
  let w_lo = make_w 0.6 in
  let s0 = Machine.make_initial (Lnp.project w_hi) x_normal y_critical in
  let s1 = (Machine.step s0 w_hi x_normal y_critical).next_state in
  let s2 = (Machine.step s1 w_lo x_normal y_critical).next_state in
  let in_meta = match s2.status with MetaReview _ -> true | _ -> false in
  if not in_meta then fail label "s2 is not in MetaReview"
  else
    let h = Instability.horizon_estimate s2 in
    if h > admissibility_lag
    then fail label
      (Printf.sprintf "horizon h=%d is not in debt zone (lag=%d); test precondition unmet"
         h admissibility_lag)
    else
      (match Instability.estimate s2 with
       | Instability.Critical -> pass label
       | Instability.Strained -> fail label "expected Critical (forced escalation), got Strained"
       | Instability.Stable   -> fail label "expected Critical (forced escalation), got Stable")

(* ── Test 20: default_mechanisms has authenticated members ────────────── *)

let () =
  let label = "default_mechanisms contains at least one authenticated mechanism" in
  let auth = List.filter (fun o -> o.authenticated) Enrichment.default_mechanisms in
  if List.length auth >= 1 then pass label
  else fail label "no authenticated mechanism in catalogue"

(* ── Test 21: viable_options with moderate horizon debt ───────────────── *)
(* Synthetic inputs: dt = 0.10, delta = 0.01, horizon = 2.
   Expected: AdaptiveSampling (latency 1 < 2; post_dt ≈ 0.104 > ε_c = 0.08)
   is at least SurvivalViable; SensorEscalation (latency 3 ≥ 2) is not.  *)

let () =
  let label = "viable_options returns AdaptiveSampling as SurvivalViable for h=2, delta=0.01" in
  let options = Enrichment.viable_options
    ~dt:0.10 ~delta:0.01 ~horizon:2
    Enrichment.default_mechanisms
  in
  if options = [] then fail label "expected at least one viable option"
  else
    let best = List.hd options in
    let ok_kind = best.Enrichment.option.kind = AdaptiveSampling in
    let ok_via  = best.Enrichment.viability <> NotViable in
    if ok_kind && ok_via then pass label
    else fail label (Printf.sprintf "best: kind=%s  viability=%s"
      (pp_enrichment_kind best.Enrichment.option.kind)
      (pp_viability best.Enrichment.viability))

(* ── Test 22: build_hdc recommends AdaptiveSampling for h=2 scenario ─── *)

let () =
  let label = "build_hdc recommends adaptive_sampling under moderate horizon debt" in
  let cert = Enrichment.build_hdc
    ~dt:0.10 ~delta:0.01 ~horizon:2
    ~steps_in_meta:1 ~required_steps:0 ~timestep:0 ()
  in
  (match cert.Enrichment.hdc_recommended with
   | Some opt when opt.kind = AdaptiveSampling -> pass label
   | Some opt -> fail label
       (Printf.sprintf "expected adaptive_sampling, got %s"
          (pp_enrichment_kind opt.kind))
   | None -> fail label "expected a recommendation, got none")

(* ── Test 23: HDC lists only viable mechanisms; unauthenticated/slow absent ── *)
(* Uses the exact parameters from the main demonstrator's first debt step:
   dt=0.0915, delta=0.0081, horizon=2.
   MatrixBoost is unauthenticated → NotViable.
   SensorEscalation has latency=3 ≥ horizon=2 → NotViable.
   Neither should appear in hdc_viable.                                  *)

let () =
  let label = "HDC excludes unauthenticated and too-slow mechanisms at h=2" in
  let cert = Enrichment.build_hdc
    ~dt:0.0915 ~delta:0.0081 ~horizon:2
    ~steps_in_meta:30 ~required_steps:1 ~timestep:49 ()
  in
  let kinds = List.map (fun ro -> ro.Enrichment.option.kind) cert.Enrichment.hdc_viable in
  if List.mem MatrixBoost kinds then
    fail label "MatrixBoost (unauthenticated) must not appear in viable list"
  else if List.mem SensorEscalation kinds then
    fail label "SensorEscalation (latency 3 ≥ horizon 2) must not appear in viable list"
  else pass label

(* ── Test 24: sensor_escalation is DurableViable when horizon >> latency ── *)
(* dt=0.20, delta=0.01, horizon=12.
   SensorEscalation: latency=3 < 12; post_dt = 0.20 - 0.03 + 0.064 = 0.234 > ε_m;
   h_post = ceil(0.154/0.01) = 16 > admissibility_lag=2  →  DurableViable.   *)

let () =
  let label = "sensor_escalation is DurableViable when dt=0.20, delta=0.01, horizon=12" in
  let options = Enrichment.viable_options
    ~dt:0.20 ~delta:0.01 ~horizon:12
    Enrichment.default_mechanisms
  in
  let sensor = List.find_opt
    (fun ro -> ro.Enrichment.option.kind = SensorEscalation)
    options
  in
  (match sensor with
   | Some ro when ro.Enrichment.viability = DurableViable -> pass label
   | Some ro ->
     fail label (Printf.sprintf "expected DurableViable, got %s"
       (pp_viability ro.Enrichment.viability))
   | None -> fail label "sensor_escalation absent from viable list")

(* ── Test 25: unauthenticated mechanism is NotViable regardless of gain ── *)
(* Provides a mechanism with a gain of 10.0 (far larger than needed to rescue
   any scenario) but authenticated=false.  The viability function must reject
   it at the authentication gate before even computing post_dt.             *)

let () =
  let label = "unauthenticated high-gain mechanism is NotViable (authentication gate)" in
  let unauth_boost = {
    kind          = MatrixBoost;
    latency_steps = 1;
    expected_gain = 10.0;
    confidence    = 0.99;
    cost          = 0.10;
    attack_risk   = 0.90;
    authenticated = false;
  } in
  let options = Enrichment.viable_options
    ~dt:0.15 ~delta:0.01 ~horizon:5
    [unauth_boost]
  in
  if options = [] then pass label
  else fail label
    (Printf.sprintf "%d mechanism(s) incorrectly marked viable" (List.length options))

(* ── Test 26: compute_opportunity_costs reports sensor_escalation drop ─── *)
(* prev: dt=0.20, delta=0.01, horizon=12, timestep=48
      → SensorEscalation is DurableViable (latency 3 < 12; post_dt=0.234 > ε_m;
        h_post=ceil(0.154/0.01)=16 > admissibility_lag=2)
   curr: dt=0.0915, delta=0.0081, horizon=2, timestep=49
      → SensorEscalation is NotViable (latency 3 ≥ 2)
   Expected: one entry for sensor_escalation with lost_rank = 3.         *)

let () =
  let label = "compute_opportunity_costs reports sensor_escalation DurableViable→NotViable" in
  let entries = Enrichment.compute_opportunity_costs
    ~dt:0.0915 ~delta:0.0081 ~horizon:2
    ~prev_dt:0.20 ~prev_delta:0.01 ~prev_horizon:12
    ~prev_timestep:48 ~timestep:49
    Enrichment.default_mechanisms
  in
  let sensor = List.find_opt
    (fun e -> e.Enrichment.oce_mechanism = SensorEscalation)
    entries
  in
  (match sensor with
   | Some e when e.Enrichment.oce_lost_rank = 3
              && e.Enrichment.oce_previous_viability = DurableViable
              && e.Enrichment.oce_current_viability  = NotViable ->
     pass label
   | Some e ->
     fail label
       (Printf.sprintf "expected lost_rank=3 DurableViable→NotViable; got %d %s→%s"
         e.Enrichment.oce_lost_rank
         (pp_viability e.Enrichment.oce_previous_viability)
         (pp_viability e.Enrichment.oce_current_viability))
   | None ->
     fail label "sensor_escalation not found in opportunity_cost entries")

(* ── Test 27: unauthenticated mechanism produces no opportunity cost ───── *)
(* MatrixBoost (unauthenticated) is NotViable at every set of parameters.
   NotViable→NotViable has rank drop 0, so no entry should be produced.   *)

let () =
  let label = "unauthenticated mechanism (MatrixBoost) produces no opportunity cost entry" in
  let entries = Enrichment.compute_opportunity_costs
    ~dt:0.0915 ~delta:0.0081 ~horizon:2
    ~prev_dt:0.20 ~prev_delta:0.01 ~prev_horizon:12
    ~prev_timestep:48 ~timestep:49
    Enrichment.default_mechanisms
  in
  let matrix = List.find_opt
    (fun e -> e.Enrichment.oce_mechanism = MatrixBoost)
    entries
  in
  (match matrix with
   | None    -> pass label
   | Some _  -> fail label "MatrixBoost (unauthenticated) must not appear in opportunity_cost")

(* ── Test 28: build_hdc with prev_params includes sensor_escalation ────── *)
(* Same parameters as tests 26/27.  build_hdc should thread prev_params
   into compute_opportunity_costs and expose the result in hdc_opportunity_cost. *)

let () =
  let label = "build_hdc with prev_params produces non-empty opportunity_cost (sensor_escalation)" in
  let cert = Enrichment.build_hdc
    ~dt:0.0915 ~delta:0.0081 ~horizon:2
    ~steps_in_meta:30 ~required_steps:1
    ~timestep:49
    ~prev_params:(Some (0.20, 0.01, 12, 48)) ()
  in
  let sensor = List.find_opt
    (fun e -> e.Enrichment.oce_mechanism = SensorEscalation)
    cert.Enrichment.hdc_opportunity_cost
  in
  (match sensor with
   | Some e when e.Enrichment.oce_lost_rank > 0 -> pass label
   | Some e ->
     fail label
       (Printf.sprintf "sensor_escalation present but lost_rank=%d" e.Enrichment.oce_lost_rank)
   | None ->
     fail label "sensor_escalation absent from hdc_opportunity_cost")

(* ── Done ────────────────────────────────────────────────────────────── *)

let () =
  print_string "\nAll 28 transition tests passed.\n"
