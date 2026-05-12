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

(* ── Done ────────────────────────────────────────────────────────────── *)

let () =
  print_string "\nAll 16 transition tests passed.\n"
