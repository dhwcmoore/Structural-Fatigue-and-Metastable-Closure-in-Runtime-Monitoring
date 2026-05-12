(* STAK-PSAL Recovery Demo.
   Shows the self-regulatory case: CLOSED → META_REVIEW → CLOSED.

   Scenario: same industrial pressure system, but with a richer context-
   enrichment budget (boost = 0.55) and slower boundary erosion (2% / step).
   The system detects boundary strain, applies context enrichment, and
   successfully re-closes — multiple times — before the run ends.

   Inputs are noise-free so the trajectory is deterministic and readable.

   Output: CSV to stdout (redirect to recovery.csv for the visualiser).    *)

open Types

let total_steps          = 100
let degradation_per_step = 0.020    (* slow erosion — enrichment can overcome it *)
let enrich_boost         = 0.55     (* strong enough to push dist back above ε_meta *)

(* ── Noise-free sensor vectors (exact, reproducible) ─────────────────── *)

let make_inputs () =
  let x = Array.init 64 (fun i -> if i = 0 then 0.20 else 0.50) in
  let y = Array.init 64 (fun i -> if i = 0 then 0.90 else 0.50) in
  x, y

(* ── Clean initial W (no noise — purely analytical) ─────────────────── *)
(* Row 0 weights dim 0 (pressure).  Row 1 weights dims 1–2 to give a
   valid 2-D projection (needed so dist is meaningful in 2D). *)

let make_clean_w () =
  let w = Array.init 2 (fun _ -> Array.make 64 0.0) in
  w.(0).(0) <- 5.0;
  w.(1).(1) <- 1.0;
  w.(1).(2) <- 1.0;
  w

(* ── CSV output ──────────────────────────────────────────────────────── *)

let print_header () =
  print_string "timestep,mx_0,mx_1,my_0,my_1,dist,status,tension,gradient,\
tension_slope,rupt_press,elast,instab\n"

let print_row (s : machine_state) =
  let d = Vec.dist s.projection_x s.projection_y in
  Printf.printf "%d,%.4f,%.4f,%.4f,%.4f,%.4f,%s,%.4f,%.4f,%.6f,%.4f,%.6f,%s\n"
    s.timestep
    s.projection_x.(0) s.projection_x.(1)
    s.projection_y.(0) s.projection_y.(1)
    d (pp_status s.status)
    s.tension.tension_score
    s.prev_gradient
    s.tension.tension_slope
    s.tension.rupture_pressure
    s.tension.recovery_elasticity
    (Instability.pp_level (Instability.estimate s))

(* ── Stderr event log ────────────────────────────────────────────────── *)

let log_event (s : machine_state) (enrichment_applied : bool) =
  match s.status with
  | MetaReview snap ->
    let enrich_note = if enrichment_applied then "  [enrichment applied]" else "" in
    Printf.eprintf
      "[t=%02d] META_REVIEW  reason=%-18s  dist=%.4f  tension=%.4f  \
instab=%-8s  elast=%+.4f%s\n"
      s.timestep snap.reason
      (Vec.dist s.projection_x s.projection_y)
      s.tension.tension_score
      (Instability.pp_level (Instability.estimate s))
      s.tension.recovery_elasticity
      enrich_note
  | Closed ->
    (* Only print CLOSED when recovering from META_REVIEW *)
    ()
  | HardRupture cert ->
    Printf.eprintf "\n[t=%02d] HARD_RUPTURE  dist=%.4f  pressure=%.4f\n"
      s.timestep
      (Vec.dist s.projection_x s.projection_y)
      cert.phenomenological_residue.rupture_pressure

(* ── Recovery cycle tracking ─────────────────────────────────────────── *)

type cycle = {
  entry_t    : int;
  recovery_t : int option;  (* Some t if returned to CLOSED, None if still META *)
}

(* ── Main loop ────────────────────────────────────────────────────────── *)

let () =
  let x, y = make_inputs () in

  assert (Admissibility.phi x = Maintain);
  assert (Admissibility.phi y = EStop);

  Printf.eprintf "STAK-PSAL Recovery Demo\n";
  Printf.eprintf "  boost=%.2f  degradation=%.3f/step  ε_meta=%.2f\n\n"
    enrich_boost degradation_per_step Types.epsilon_meta;

  let w      = ref (make_clean_w ()) in
  let state  = ref (Machine.make_initial (Lnp.project !w) x y) in
  let cycles = ref [] in

  Printf.eprintf "  Initial dist: %.4f\n\n" (Vec.dist !state.projection_x !state.projection_y);

  print_header ();
  print_row !state;

  let prev_meta     = ref false in
  let in_cycle      = ref false in
  let cycle_entry_t = ref 0 in

  for _t = 1 to total_steps do
    let result = Machine.step ~boost:enrich_boost !state !w x y in
    state := result.next_state;

    let now_meta = match !state.status with MetaReview _ -> true | _ -> false in

    (* Enrichment fires only on CLOSED → META_REVIEW transition *)
    let enrichment_applied =
      match result.enrichment with
      | Some w' when now_meta && not !prev_meta ->
        w := w'; true
      | _ -> false
    in

    (* Track recovery cycles *)
    if now_meta && not !prev_meta then begin
      in_cycle      := true;
      cycle_entry_t := !state.timestep;
      Printf.eprintf "── Cycle entry at t=%d  dist=%.4f ──\n"
        !state.timestep (Vec.dist !state.projection_x !state.projection_y)
    end;
    if not now_meta && !prev_meta && !in_cycle then begin
      let c = { entry_t = !cycle_entry_t; recovery_t = Some !state.timestep } in
      cycles := c :: !cycles;
      in_cycle := false;
      Printf.eprintf "── Recovery at t=%d  (cycle lasted %d step(s))  dist=%.4f ──\n"
        !state.timestep (!state.timestep - c.entry_t)
        (Vec.dist !state.projection_x !state.projection_y)
    end;

    print_row !state;
    log_event !state enrichment_applied;
    prev_meta := now_meta;

    (match !state.status with
     | HardRupture _ -> ()   (* stop tracking; loop continues but won't update state *)
     | _ ->
       w := Lnp.degrade !w degradation_per_step)
  done;

  (* Summary *)
  let n_recoveries =
    List.length (List.filter (fun c -> c.recovery_t <> None) !cycles)
  in
  Printf.eprintf "\n═══ Summary ═══\n";
  Printf.eprintf "  Total steps      : %d\n" !state.timestep;
  Printf.eprintf "  META_REVIEW cycles: %d\n" (List.length !cycles);
  Printf.eprintf "  Successful recoveries: %d\n" n_recoveries;
  Printf.eprintf "  Final status     : %s\n" (pp_status !state.status);
  Printf.eprintf "  Final dist       : %.4f\n"
    (Vec.dist !state.projection_x !state.projection_y)
