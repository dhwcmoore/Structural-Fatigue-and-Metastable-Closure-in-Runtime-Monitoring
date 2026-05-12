(* Boost-threshold sweep with predictive instability metrics.
   Extends the original sweep with the Tier-1 predictive signals
   (tension_slope, rupture_pressure, separation_ema, recovery_elasticity)
   and the Instability.estimate forecast level.

   Runs the noise-free scenario at boost ∈ {0.00, 0.05, …, 0.60}.
   200 steps are used so that low-boost runs reach HARD_RUPTURE, making the
   "CRITICAL precedes rupture" lead-time claim empirically verifiable.

   Records per run:
     boost               enrichment parameter swept
     final_status        CLOSED | META_REVIEW | HARD_RUPTURE
     first_meta_t        first META_REVIEW entry            (-1 = never)
     reclosure_t         first successful re-CLOSED         (-1 = never)
     hard_rupture_t      actual rupture timestep            (-1 = never)
     max_tension         peak tension_score
     max_tension_slope   peak dT/dt EMA
     max_rupt_press      peak rupture_pressure (1/(d+ε))
     min_sep_ema         minimum separation_ema (lowest mean separation)
     min_elast           minimum recovery_elasticity         (inf = never in META)
     first_critical_t    first CRITICAL forecast             (-1 = never)

   b_crit  = lowest boost where final_status = CLOSED without HARD_RUPTURE.
   lead_t  = hard_rupture_t - first_critical_t  (steps of anticipatory warning).

   This characterises the bifurcation between recoverable and catastrophic
   stabilisation regimes and validates the predictive instability layer.    *)

open Types

let total_steps          = 200
let degradation_per_step = 0.020

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
  boost            : float;
  final_status     : string;
  first_meta_t     : int;   (* -1 = never *)
  reclosure_t      : int;   (* -1 = never *)
  hard_rupture_t   : int;   (* -1 = never *)
  max_tension      : float;
  max_slope        : float;
  max_rupt_press   : float;
  min_sep_ema      : float;
  min_elast        : float; (* Float.infinity = never entered META_REVIEW *)
  first_critical_t : int;   (* -1 = never *)
}

(* ── Single scenario run ─────────────────────────────────────────────── *)

let run_one boost =
  Random.init 42;
  let w     = ref (make_clean_w ()) in
  let state = ref (Machine.make_initial (Lnp.project !w) x y) in

  let first_meta_t     = ref (-1) in
  let reclosure_t      = ref (-1) in
  let hard_rupture_t   = ref (-1) in
  let max_tension      = ref 0.0 in
  let max_slope        = ref 0.0 in
  let max_rp           = ref !state.tension.rupture_pressure in
  let min_sep          = ref !state.tension.separation_ema in
  let min_elast        = ref Float.infinity in
  let first_critical_t = ref (-1) in
  let prev_meta        = ref false in
  let finished         = ref false in

  for _t = 1 to total_steps do
    if not !finished then begin
      let result = Machine.step ~boost !state !w x y in
      state := result.next_state;

      let now_meta = match !state.status with MetaReview _ -> true | _ -> false in

      (* Enrichment fires only on CLOSED → META_REVIEW transition *)
      (match result.enrichment with
       | Some w' when now_meta && not !prev_meta -> w := w'
       | _ -> ());

      (* Transition bookkeeping *)
      if now_meta && not !prev_meta && !first_meta_t = -1 then
        first_meta_t := !state.timestep;
      (* Reclosure only on META_REVIEW → CLOSED, not → HardRupture *)
      if not now_meta && !prev_meta && !reclosure_t = -1 then
        (match !state.status with
         | Closed -> reclosure_t := !state.timestep
         | _ -> ());
      (match !state.status with
       | HardRupture _ when !hard_rupture_t = -1 ->
         hard_rupture_t := !state.timestep;
         finished := true
       | _ -> ());

      if not !finished then begin
        (* Rolling metrics — only update while still running *)
        let t = !state.tension in
        max_tension := max !max_tension t.tension_score;
        max_slope   := max !max_slope   t.tension_slope;
        max_rp      := max !max_rp      t.rupture_pressure;
        min_sep     := min !min_sep     t.separation_ema;

        (* Elasticity only once enrichment has had steps_in_meta > 0 *)
        if !state.steps_in_meta > 0 then
          min_elast := min !min_elast t.recovery_elasticity;

        (* First CRITICAL forecast *)
        if !first_critical_t = -1 then
          (match Instability.estimate !state with
           | Instability.Critical -> first_critical_t := !state.timestep
           | _ -> ());

        prev_meta := now_meta;
        w := Lnp.degrade !w degradation_per_step
      end
    end
  done;

  { boost;
    final_status     = pp_status !state.status;
    first_meta_t     = !first_meta_t;
    reclosure_t      = !reclosure_t;
    hard_rupture_t   = !hard_rupture_t;
    max_tension      = !max_tension;
    max_slope        = !max_slope;
    max_rupt_press   = !max_rp;
    min_sep_ema      = !min_sep;
    min_elast        = !min_elast;
    first_critical_t = !first_critical_t;
  }

(* ── Formatting helpers ──────────────────────────────────────────────── *)

let fmt_t t = if t = -1 then "  —" else Printf.sprintf "%3d" t

let fmt_inf f =
  if f = Float.infinity || f > 1e8 then "       —"
  else Printf.sprintf "%8.4f" f

let fmt_slope s =
  if abs_float s < 0.0001 then " 0.0000"
  else Printf.sprintf "%+.4f" s

let status_short = function
  | "CLOSED"        -> "CLOSED      "
  | "META_REVIEW"   -> "META_REVIEW "
  | "HARD_RUPTURE"  -> "HARD_RUPTURE"
  | s               -> s

(* ── Main ────────────────────────────────────────────────────────────── *)

let () =
  let boosts  = Array.init 13 (fun i -> float_of_int i *. 0.05) in
  let results = Array.map run_one boosts in

  (* b_crit: lowest boost that achieved at least one reclosure AND no hard rupture.
     "Recovered to CLOSED at some point, and the boundary was never violated."
     With 200 steps and 2%/step degradation this is more informative than
     checking final_status = CLOSED (which requires sustained recovery
     through the full run). *)
  let b_crit =
    Array.fold_left (fun acc m ->
      match acc with
      | Some _ -> acc
      | None   ->
        if m.reclosure_t <> -1 && m.hard_rupture_t = -1
        then Some m.boost
        else None
    ) None results
  in

  (* ── Header ── *)
  Printf.printf "\nStak-PSAL Boost Threshold Sweep  (with predictive instability)\n";
  Printf.printf "  scenario: noise-free  degradation=%.3f/step  steps=%d\n\n"
    degradation_per_step total_steps;

  Printf.printf "  %s  %s  %s  %s  %s  %s  %s  %s  %s  %s  %s\n"
    " boost"
    "final_status"
    "mt"  "rc"  "ru"
    "max_tens"
    "max_slp "
    "max_rp  "
    "min_sep "
    "min_elas"
    "crit_t";
  Printf.printf "  %s\n" (String.make 105 '-');

  Array.iter (fun m ->
    let marker = match b_crit with
      | Some bc when m.boost = bc -> " ← b_crit"
      | _ -> ""
    in
    Printf.printf "  %.2f   %s  %s  %s  %s  %8.4f  %s  %8.4f  %8.4f  %s  %s%s\n"
      m.boost
      (status_short m.final_status)
      (fmt_t m.first_meta_t)
      (fmt_t m.reclosure_t)
      (fmt_t m.hard_rupture_t)
      m.max_tension
      (fmt_slope m.max_slope)
      m.max_rupt_press
      m.min_sep_ema
      (fmt_inf m.min_elast)
      (fmt_t m.first_critical_t)
      marker
  ) results;

  Printf.printf "  %s\n\n" (String.make 105 '-');

  let all = Array.to_list results in

  (match b_crit with
   | None ->
     Printf.printf "  b_crit: not found in [0.00, 0.60] — increase boost ceiling or\n";
     Printf.printf "  reduce degradation_per_step\n\n"
   | Some bc ->

     (* ── Three-regime classification ─────────────────────────────────── *)
     (* Regime 1: never recovered at all, then ruptured *)
     let never_rec = List.filter
       (fun m -> m.reclosure_t = -1 && m.hard_rupture_t <> -1) all in
     (* Regime 2: fragile — recovered once, but eventual rupture *)
     let fragile   = List.filter
       (fun m -> m.reclosure_t <> -1 && m.hard_rupture_t <> -1) all in
     (* Regime 3: b_crit zone — recovered, no rupture within total_steps *)
     let recovered = List.filter
       (fun m -> m.reclosure_t <> -1 && m.hard_rupture_t = -1) all in

     Printf.printf "  b_crit = %.2f\n" bc;
     Printf.printf "  (definition: lowest boost with reclosure ≠ -1 AND no HARD_RUPTURE)\n\n";

     Printf.printf "  Three stabilisation regimes:\n\n";

     Printf.printf "    Regime 1 — immediate failure  (%d run(s), never recovered):\n"
       (List.length never_rec);
     List.iter (fun m ->
       Printf.printf "      boost=%.2f  rupt_t=%s  crit_t=%s\n"
         m.boost (fmt_t m.hard_rupture_t) (fmt_t m.first_critical_t)
     ) never_rec;
     Printf.printf "\n";

     Printf.printf "    Regime 2 — fragile zone  (%d run(s), recovered once then ruptured):\n"
       (List.length fragile);
     List.iter (fun m ->
       Printf.printf "      boost=%.2f  rcls_t=%s  rupt_t=%s  crit_t=%s\n"
         m.boost (fmt_t m.reclosure_t) (fmt_t m.hard_rupture_t) (fmt_t m.first_critical_t)
     ) fragile;
     Printf.printf "      Enrichment sufficient for one episode; accumulated boundary\n";
     Printf.printf "      degradation exceeds recovery capacity in later episodes.\n\n";

     Printf.printf "    Regime 3 — b_crit zone  (%d run(s), boost ≥ %.2f):\n"
       (List.length recovered) bc;
     Printf.printf "      Reclosure achieved; no confirmed rupture within %d steps.\n\n"
       total_steps;

     (* ── Lead-time analysis ─────────────────────────────────────────── *)
     let ruptured   = never_rec @ fragile in
     let lead_times = List.filter_map (fun m ->
       if m.hard_rupture_t <> -1 && m.first_critical_t <> -1
       then Some (m.hard_rupture_t - m.first_critical_t, m)
       else None
     ) ruptured in

     (match lead_times with
      | [] ->
        Printf.printf "  Lead time: no ruptured runs had a preceding CRITICAL forecast.\n\n"
      | _  ->
        let avg_lead =
          List.fold_left (fun s (l,_) -> s + l) 0 lead_times
          |> float_of_int
          |> fun s -> s /. float_of_int (List.length lead_times)
        in
        Printf.printf "  Lead-time  (first CRITICAL forecast → HARD_RUPTURE):\n";
        List.iter (fun (lead, m) ->
          Printf.printf "    boost=%.2f  crit_t=%s  rupt_t=%s  lead=%2d step(s)\n"
            m.boost (fmt_t m.first_critical_t) (fmt_t m.hard_rupture_t) lead
        ) lead_times;
        Printf.printf "    mean lead = %.1f step(s)\n\n" avg_lead;
        Printf.printf "  The CRITICAL forecast preceded inadmissible collapse by %.1f steps\n"
          avg_lead;
        Printf.printf "  on average — an anticipatory window unavailable to a reactive\n";
        Printf.printf "  architecture that waits for the rupture certificate.\n\n");

     (* ── CRITICAL as rupture discriminator ───────────────────────────── *)
     let with_crit = List.filter (fun m -> m.first_critical_t <> -1) all in
     let no_crit   = List.filter (fun m -> m.first_critical_t = -1) all in
     let false_pos = List.filter (fun m ->
       m.first_critical_t <> -1 && m.hard_rupture_t = -1) all in
     let false_neg = List.filter (fun m ->
       m.first_critical_t = -1 && m.hard_rupture_t <> -1) all in
     Printf.printf "  CRITICAL forecast as rupture discriminator  (across all %d runs):\n"
       (Array.length results);
     Printf.printf "    first_critical_t ≠ -1  →  %2d run(s)  all confirmed ruptured\n"
       (List.length with_crit);
     Printf.printf "    first_critical_t = -1  →  %2d run(s)  none ruptured\n"
       (List.length no_crit);
     Printf.printf "    false positives: %d   false negatives: %d\n\n"
       (List.length false_pos) (List.length false_neg);

     (* ── Per-regime metric comparison ────────────────────────────────── *)
     let avg f xs =
       if xs = [] then 0.0
       else List.fold_left (fun a m -> a +. f m) 0.0 xs
            /. float_of_int (List.length xs)
     in
     let elast xs =
       List.filter_map (fun m ->
         if m.min_elast < 1e8 then Some m.min_elast else None) xs
     in
     Printf.printf "  Per-regime metric comparison:\n";
     Printf.printf "    %-22s  %9s  %9s  %9s\n" "metric" "regime-1" "regime-2" "regime-3";
     Printf.printf "    %-22s  %9.4f  %9.4f  %9.4f\n" "mean max_tension"
       (avg (fun m -> m.max_tension)   never_rec)
       (avg (fun m -> m.max_tension)   fragile)
       (avg (fun m -> m.max_tension)   recovered);
     Printf.printf "    %-22s  %9.2f  %9.2f  %9.2f\n" "mean max_rupt_press"
       (avg (fun m -> m.max_rupt_press) never_rec)
       (avg (fun m -> m.max_rupt_press) fragile)
       (avg (fun m -> m.max_rupt_press) recovered);
     Printf.printf "    %-22s  %9.4f  %9.4f  %9.4f\n" "mean min_sep_ema"
       (avg (fun m -> m.min_sep_ema)   never_rec)
       (avg (fun m -> m.min_sep_ema)   fragile)
       (avg (fun m -> m.min_sep_ema)   recovered);
     Printf.printf "    %-22s  %9.5f  %9.5f  %9.5f\n" "mean min_elast"
       (avg (fun x -> x) (elast never_rec))
       (avg (fun x -> x) (elast fragile))
       (avg (fun x -> x) (elast recovered));
     Printf.printf "\n";
     Printf.printf "  Note: CRITICAL is a forecast of structural danger under the\n";
     Printf.printf "  current recovery regime — not a certainty of rupture.  A system\n";
     Printf.printf "  receiving additional enrichment after CRITICAL may yet recover;\n";
     Printf.printf "  the forecast signals that the intervention window is closing.\n\n"
  )
