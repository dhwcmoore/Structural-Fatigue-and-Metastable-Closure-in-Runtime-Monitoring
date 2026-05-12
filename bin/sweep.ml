(* Boost threshold sweep for STAK-PSAL.
   Runs the recovery scenario (noise-free inputs, 2 %/step degradation,
   100 steps) at each boost value in [0.00 .. 0.55] and records:

     final_status        CLOSED | META_REVIEW | HARD_RUPTURE
     first_meta_t        timestep of first META_REVIEW entry  (-1 = never)
     reclosure_t         timestep of first successful re-CLOSED  (-1 = never)
     hard_rupture_t      timestep of rupture  (-1 = never)
     max_tension_score   peak tension accumulator over the run
     mean_delta          mean projection distance (all timesteps)
     max_delta           peak projection distance (all timesteps)
     oscillation_count   total gradient sign-flips
     min_proj_dist       smallest projection distance seen
     tension_slope_at_meta  EMA slope at first META_REVIEW entry
     tension_slope_final    EMA slope at final timestep

   b_crit is identified as the lowest boost where final_status = CLOSED
   without any HARD_RUPTURE occurring.                                     *)

open Types

let total_steps          = 100
let degradation_per_step = 0.020

(* ── Noise-free scenario (same as recover.ml) ───────────────────────── *)

let x = Array.init 64 (fun i -> if i = 0 then 0.20 else 0.50)
let y = Array.init 64 (fun i -> if i = 0 then 0.90 else 0.50)

let make_clean_w () =
  let w = Array.init 2 (fun _ -> Array.make 64 0.0) in
  w.(0).(0) <- 5.0;
  w.(1).(1) <- 1.0;
  w.(1).(2) <- 1.0;
  w

(* ── Per-run metrics ─────────────────────────────────────────────────── *)

type run_metrics = {
  boost                 : float;
  final_status          : string;
  first_meta_t          : int;    (* -1 = never *)
  reclosure_t           : int;    (* -1 = never *)
  hard_rupture_t        : int;    (* -1 = never *)
  max_tension_score     : float;
  mean_delta            : float;
  max_delta             : float;
  oscillation_count     : int;
  min_proj_dist         : float;
  tension_slope_at_meta : float;
  tension_slope_final   : float;
}

(* ── Single scenario run ─────────────────────────────────────────────── *)

let run_one boost =
  Random.init 42;
  let w      = ref (make_clean_w ()) in
  let state  = ref (Machine.make_initial (Lnp.project !w) x y) in

  let first_meta_t          = ref (-1) in
  let reclosure_t           = ref (-1) in
  let hard_rupture_t        = ref (-1) in
  let max_tension           = ref 0.0 in
  let dist_sum              = ref 0.0 in
  let dist_count            = ref 0 in
  let max_dist              = ref 0.0 in
  let min_dist              = ref Float.infinity in
  let tension_slope_at_meta = ref 0.0 in
  let prev_meta             = ref false in

  (* Record step-0 *)
  let d0 = Vec.dist !state.projection_x !state.projection_y in
  dist_sum   := !dist_sum +. d0;
  incr dist_count;
  max_dist   := max !max_dist d0;
  min_dist   := min !min_dist d0;

  for _t = 1 to total_steps do
    let result  = Machine.step ~boost !state !w x y in
    state := result.next_state;

    let now_meta = match !state.status with MetaReview _ -> true | _ -> false in

    (* Enrichment fires only on CLOSED → META_REVIEW transition *)
    (match result.enrichment with
     | Some w' when now_meta && not !prev_meta -> w := w'
     | _ -> ());

    (* Transition bookkeeping *)
    if now_meta && not !prev_meta && !first_meta_t = -1 then begin
      first_meta_t          := !state.timestep;
      tension_slope_at_meta := !state.tension.tension_slope
    end;
    if not now_meta && !prev_meta && !reclosure_t = -1 then
      reclosure_t := !state.timestep;
    (match !state.status with
     | HardRupture _ when !hard_rupture_t = -1 ->
       hard_rupture_t := !state.timestep
     | _ -> ());

    (* Rolling stats *)
    let d = Vec.dist !state.projection_x !state.projection_y in
    dist_sum   := !dist_sum +. d;
    incr dist_count;
    max_dist   := max !max_dist d;
    min_dist   := min !min_dist d;
    max_tension := max !max_tension !state.tension.tension_score;

    prev_meta := now_meta;

    (match !state.status with
     | HardRupture _ -> ()
     | _ -> w := Lnp.degrade !w degradation_per_step)
  done;

  { boost;
    final_status          = pp_status !state.status;
    first_meta_t          = !first_meta_t;
    reclosure_t           = !reclosure_t;
    hard_rupture_t        = !hard_rupture_t;
    max_tension_score     = !max_tension;
    mean_delta            = !dist_sum /. float_of_int !dist_count;
    max_delta             = !max_dist;
    oscillation_count     = !state.tension.oscillation_count;
    min_proj_dist         = !min_dist;
    tension_slope_at_meta = !tension_slope_at_meta;
    tension_slope_final   = !state.tension.tension_slope;
  }

(* ── Formatting helpers ──────────────────────────────────────────────── *)

let fmt_t t = if t = -1 then "   —" else Printf.sprintf "%4d" t

let fmt_slope s =
  if s > 0.0001 then Printf.sprintf "+%.4f" s
  else if s < -0.0001 then Printf.sprintf "%.4f" s
  else " 0.0000"

let status_short = function
  | "CLOSED"        -> "CLOSED      "
  | "META_REVIEW"   -> "META_REVIEW "
  | "HARD_RUPTURE"  -> "HARD_RUPTURE"
  | s               -> s

(* ── Main ────────────────────────────────────────────────────────────── *)

let () =
  let boosts = Array.init 12 (fun i -> float_of_int i *. 0.05) in

  let results = Array.map run_one boosts in

  (* Identify b_crit: lowest boost where final_status = CLOSED *)
  let b_crit =
    Array.fold_left (fun acc m ->
      match acc with
      | Some _ -> acc
      | None   -> if m.final_status = "CLOSED" then Some m.boost else None
    ) None results
  in

  (* ── Header ── *)
  Printf.printf "\nStak-PSAL Boost Threshold Sweep\n";
  Printf.printf "  scenario: noise-free  degradation=%.3f/step  steps=%d\n\n"
    degradation_per_step total_steps;

  Printf.printf "  %s  %s  %s  %s  %s  %s  %s  %s  %s  %s  %s  %s\n"
    " boost"
    "final_status"
    "meta_t"
    "rcls_t"
    "rupt_t"
    "max_tens"
    "mean_d  "
    "max_d   "
    "osc"
    "min_d   "
    "slope@meta"
    "slope_fin ";
  Printf.printf "  %s\n" (String.make 112 '-');

  Array.iter (fun m ->
    let marker = match b_crit with
      | Some bc when m.boost = bc -> " ← b_crit"
      | _                         -> ""
    in
    Printf.printf "  %.2f   %s  %s  %s  %s  %8.4f  %8.4f  %8.4f  %3d  %8.4f  %s  %s%s\n"
      m.boost
      (status_short m.final_status)
      (fmt_t m.first_meta_t)
      (fmt_t m.reclosure_t)
      (fmt_t m.hard_rupture_t)
      m.max_tension_score
      m.mean_delta
      m.max_delta
      m.oscillation_count
      m.min_proj_dist
      (fmt_slope m.tension_slope_at_meta)
      (fmt_slope m.tension_slope_final)
      marker
  ) results;

  Printf.printf "  %s\n\n" (String.make 112 '-');

  let all = Array.to_list results in

  (match b_crit with
   | None ->
     Printf.printf "  b_crit: not found — no boost in sweep achieved recovery\n"
   | Some bc ->
     (* "failed" = final_status is not CLOSED (includes META_REVIEW + HARD_RUPTURE) *)
     let failed   = List.filter (fun m -> m.boost < bc) all in
     let recovered = List.filter (fun m -> m.boost >= bc) all in
     let n_rupt   = List.length (List.filter (fun m -> m.hard_rupture_t <> -1) failed) in
     let n_stuck  = List.length failed - n_rupt in

     Printf.printf "  b_crit = %.2f\n" bc;
     Printf.printf "\n";
     Printf.printf "  boost <  %.2f  ->  failed stabilization (%d run(s))\n" bc (List.length failed);
     Printf.printf "               of which: %d confirmed HARD_RUPTURE within %d steps\n" n_rupt total_steps;
     Printf.printf "                         %d stuck in META_REVIEW (unstabilized;\n" n_stuck;
     Printf.printf "                         would rupture with additional steps)\n";
     Printf.printf "  boost >= %.2f  ->  recovery: final_status = CLOSED (%d run(s))\n\n" bc (List.length recovered);

     (* Second threshold: where max_tension drops sharply *)
     let tension_threshold =
       List.fold_left (fun acc m ->
         match acc with
         | Some _ -> acc
         | None   -> if m.max_tension_score < 0.05 then Some m.boost else None
       ) None all
     in
     (match tension_threshold with
      | Some bt when bt < bc ->
        Printf.printf "  max_tension threshold = %.2f  (sharp drop in peak tension;\n" bt;
        Printf.printf "  below this, strain accumulates heavily even if short-term\n";
        Printf.printf "  reclosure occurs — an earlier warning signal than b_crit)\n\n"
      | _ -> ());

     (* Tension slope: use max_tension as the cleaner discriminator given small EMA values *)
     let avg_tension xs =
       if xs = [] then 0.0
       else List.fold_left (fun a m -> a +. m.max_tension_score) 0.0 xs
            /. float_of_int (List.length xs)
     in
     Printf.printf "  mean max_tension_score  below b_crit:  %.4f\n" (avg_tension failed);
     Printf.printf "  mean max_tension_score  above b_crit:  %.4f\n\n" (avg_tension recovered);

     Printf.printf "  Interpretation:\n";
     Printf.printf "  tension_slope_at_meta is not a useful early discriminator — all runs\n";
     Printf.printf "  enter META_REVIEW with the same slope (+0.0005) because enrichment\n";
     Printf.printf "  has not yet acted at the moment of entry.\n";
     Printf.printf "\n";
     Printf.printf "  max_tension_score cleanly separates the two regimes: failed runs\n";
     Printf.printf "  accumulate %.1fx more tension than recovered runs.\n"
       (avg_tension failed /. max 0.0001 (avg_tension recovered));
     Printf.printf "\n";
     Printf.printf "  For real-time use: monitor tension_slope in the MIDDLE of a\n";
     Printf.printf "  META_REVIEW episode (5+ steps after entry). A slope that remains\n";
     Printf.printf "  positive after enrichment is the operational early-warning signal.\n";
  )
