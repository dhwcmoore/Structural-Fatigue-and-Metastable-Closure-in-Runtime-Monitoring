(* STAK-PSAL Toy Demonstrator.
   Scenario: industrial pressure-monitoring system.
     x = "Normal Pressure"  (dim-0 = 0.20)  →  Φ(x) = Maintain
     y = "Critical Surge"   (dim-0 = 0.90)  →  Φ(y) = EStop

   The boundary governing the LNP projection is progressively eroded.
   Context enrichment (META_REVIEW) fights the erosion.  When the
   projection distance finally collapses below ε_collapse while
   Φ(x) ≠ Φ(y), a Hard Rupture certificate is emitted.

   Output: CSV to stdout (redirect to trajectory.csv for the visualiser);
           rupture_certificate.json written to the current directory.    *)

open Types

let total_steps          = 80
let degradation_per_step = 0.050   (* fraction of W[0][0] lost per step *)

(* ── Build the two fixed sensor vectors ─────────────────────────────── *)

let make_inputs () =
  let x = Array.init 64 (fun i ->
    if i = 0 then 0.20
    else 0.50 +. Vec.rand_gauss () *. 0.15)
  in
  let y = Array.init 64 (fun i ->
    if i = 0 then 0.90
    else 0.50 +. Vec.rand_gauss () *. 0.15)
  in
  x, y

(* ── CSV header / row ────────────────────────────────────────────────── *)

let print_header () =
  print_string "timestep,mx_0,mx_1,my_0,my_1,dist,status,tension,gradient,\
tension_slope,rupt_press,elast,instab\n"

let print_row (s : machine_state) =
  let d = Vec.dist s.projection_x s.projection_y in
  Printf.printf "%d,%.4f,%.4f,%.4f,%.4f,%.4f,%s,%.4f,%.4f,%.6f,%.4f,%.6f,%s\n"
    s.timestep
    s.projection_x.(0) s.projection_x.(1)
    s.projection_y.(0) s.projection_y.(1)
    d
    (pp_status s.status)
    s.tension.tension_score
    s.prev_gradient
    s.tension.tension_slope
    s.tension.rupture_pressure
    s.tension.recovery_elasticity
    (Instability.pp_level (Instability.estimate s))

(* ── Stderr event log ────────────────────────────────────────────────── *)

let log_event (s : machine_state) =
  match s.status with
  | MetaReview snap ->
    Printf.eprintf
      "[t=%02d] META_REVIEW  reason=%-18s  dist=%.4f  \
       tension=%.4f  boundary_revisions=%d\n"
      s.timestep snap.reason
      (Vec.dist s.projection_x s.projection_y)
      s.tension.tension_score
      s.tension.boundary_revisions
  | HardRupture cert ->
    let w = cert.witness in
    Printf.eprintf
      "\n╔══════════════════════════════════════════════╗\n";
    Printf.eprintf "║           HARD RUPTURE DETECTED              ║\n";
    Printf.eprintf "╚══════════════════════════════════════════════╝\n";
    Printf.eprintf "  t = %d\n" s.timestep;
    Printf.eprintf "  M(x) = [%.4f, %.4f]   Φ(x) = %s\n"
      w.projection_x.(0) w.projection_x.(1) (pp_action w.action_x);
    Printf.eprintf "  M(y) = [%.4f, %.4f]   Φ(y) = %s\n"
      w.projection_y.(0) w.projection_y.(1) (pp_action w.action_y);
    Printf.eprintf "  Rupture pressure   : %.4f\n"
      cert.phenomenological_residue.rupture_pressure;
    Printf.eprintf "  Boundary revisions : %d\n"
      cert.phenomenological_residue.interval_widening_frequency;
    Printf.eprintf "  %s\n"
      cert.phenomenological_residue.inheritance_summary
  | Closed -> ()

(* ── Main loop ────────────────────────────────────────────────────────── *)

let () =
  Random.init 42;

  let x, y = make_inputs () in

  (* Sanity: confirm the safety function partitions the inputs correctly *)
  assert (Admissibility.phi x = Maintain);
  assert (Admissibility.phi y = EStop);

  Printf.eprintf "STAK-PSAL Toy Demonstrator\n";
  Printf.eprintf "  φ(x) = Maintain  (Normal Pressure,  dim-0 = %.2f)\n" x.(0);
  Printf.eprintf "  φ(y) = EStop     (Critical Surge,   dim-0 = %.2f)\n" y.(0);
  Printf.eprintf "  ε_collapse = %.2f   ε_meta = %.2f\n\n"
    Types.epsilon_collapse Types.epsilon_meta;

  let w     = ref (Lnp.make_initial_w ()) in
  let state = ref (Machine.make_initial (Lnp.project !w) x y) in

  Printf.eprintf "  Initial projection distance: %.4f\n\n"
    (Vec.dist !state.projection_x !state.projection_y);

  print_header ();
  print_row !state;

  let finished  = ref false in
  let prev_meta = ref false in   (* whether previous step was META_REVIEW *)

  for _t = 1 to total_steps do
    if not !finished then begin
      let result = Machine.step !state !w x y in
      state := result.next_state;

      (* Context enrichment fires only on CLOSED → META_REVIEW transitions.
         Staying in META_REVIEW does not re-trigger enrichment; the boundary
         erosion is relentless once the system can no longer recover. *)
      let now_meta = match !state.status with MetaReview _ -> true | _ -> false in
      (match result.enrichment with
       | Some w' when now_meta && not !prev_meta -> w := w'
       | _ -> ());
      prev_meta := now_meta;

      print_row !state;
      log_event !state;

      (match !state.status with
       | HardRupture cert ->
         let json = Certificate.to_json cert in
         let oc = open_out "rupture_certificate.json" in
         output_string oc json;
         close_out oc;
         Printf.eprintf "\n  Certificate → rupture_certificate.json\n";
         finished := true
       | _ -> ());

      (* Degrade boundary after each step (environment-side erosion) *)
      if not !finished then
        w := Lnp.degrade !w degradation_per_step
    end
  done;

  if not !finished then
    Printf.eprintf "\n[note] No rupture within %d steps.\n" total_steps
