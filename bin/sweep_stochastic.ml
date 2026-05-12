(* Stochastic multi-seed boost × degradation bifurcation sweep.
   Extends sweep_boost.ml with:
     - Noisy inputs and W (like the rupture demo) for empirical robustness.
     - 100 seeds per (boost, degrade) pair to build a distribution.
     - Five degradation rates so b_crit can be charted across the parameter space.
     - Cumulative fatigue functional F_n that accumulates strain residue across
       META_REVIEW episodes, formalising compound degradation:

         F_step += |ΔT| + max(0, −E_r)·0.1 + max(0, RP − 5.0)·0.01

   Four regimes are assigned per run:
     0 — never entered META_REVIEW (or stuck, no rupture)
     1 — immediate failure: never recovered, ruptured
     2 — fragile zone: recovered at least once, then ruptured
     3 — b_crit zone: recovered, no rupture within total_steps

   Output: CSV to stdout  (redirect to sweep_stochastic.csv for the viz).
   Summary: regime distribution per parameter cell to stderr.               *)

open Types

let total_steps = 200
let n_seeds     = 100

let boosts   = Array.init 13 (fun i -> float_of_int i *. 0.05)
let degrades = [| 0.010; 0.015; 0.020; 0.025; 0.030 |]

(* ── Per-run result ──────────────────────────────────────────────────── *)

type run_result = {
  seed    : int;
  boost   : float;
  degrade : float;
  regime  : int;    (* 0–3 as described above *)
  n_meta  : int;    (* number of META_REVIEW entries *)
  rcls_t  : int;    (* -1 = never *)
  rupt_t  : int;    (* -1 = never *)
  crit_t  : int;    (* -1 = never *)
  max_tens  : float;
  max_rp    : float;
  min_sep   : float;
  min_elast : float;  (* Float.infinity = never in META_REVIEW *)
  fatigue   : float;  (* cumulative fatigue functional F_n *)
}

(* ── Single run ──────────────────────────────────────────────────────── *)

let run_one ~degrade ~boost ~seed =
  Random.init seed;

  (* Noisy inputs: dim 0 is fixed (determines Φ); dims 1–63 are perturbed *)
  let x = Array.init 64 (fun i ->
    if i = 0 then 0.20 else 0.50 +. Vec.rand_gauss () *. 0.15) in
  let y = Array.init 64 (fun i ->
    if i = 0 then 0.90 else 0.50 +. Vec.rand_gauss () *. 0.15) in

  let w     = ref (Lnp.make_initial_w ()) in
  let state = ref (Machine.make_initial (Lnp.project !w) x y) in

  let rcls_t    = ref (-1) in
  let rupt_t    = ref (-1) in
  let crit_t    = ref (-1) in
  let n_meta    = ref 0 in
  let max_tens  = ref 0.0 in
  let max_rp    = ref !state.tension.rupture_pressure in
  let min_sep   = ref !state.tension.separation_ema in
  let min_elast = ref Float.infinity in
  let fatigue   = ref 0.0 in
  let prev_meta = ref false in
  let finished  = ref false in
  let prev_score = ref !state.tension.tension_score in

  for _t = 1 to total_steps do
    if not !finished then begin
      let result = Machine.step ~boost !state !w x y in
      state := result.next_state;

      let now_meta = match !state.status with MetaReview _ -> true | _ -> false in

      (* Enrichment only on CLOSED → META_REVIEW transition *)
      (match result.enrichment with
       | Some w' when now_meta && not !prev_meta -> w := w'
       | _ -> ());

      (* Transition bookkeeping *)
      if now_meta && not !prev_meta then
        incr n_meta;
      if not now_meta && !prev_meta && !rcls_t = -1 then
        (match !state.status with
         | Closed -> rcls_t := !state.timestep
         | _ -> ());
      (match !state.status with
       | HardRupture _ when !rupt_t = -1 ->
         rupt_t  := !state.timestep;
         finished := true
       | _ -> ());

      if not !finished then begin
        let t = !state.tension in

        (* Rolling metrics *)
        max_tens  := max !max_tens  t.tension_score;
        max_rp    := max !max_rp    t.rupture_pressure;
        min_sep   := min !min_sep   t.separation_ema;
        if !state.steps_in_meta > 0 then
          min_elast := min !min_elast t.recovery_elasticity;

        (* Cumulative fatigue in META_REVIEW:
           F += |ΔT| + max(0,−E_r)·0.1 + max(0, RP−5)·0.01   *)
        if now_meta then begin
          let delta_t  = abs_float (t.tension_score -. !prev_score) in
          let elast_l  = max 0.0 (-. t.recovery_elasticity) in
          let press_x  = max 0.0 (t.rupture_pressure -. 5.0) in
          fatigue := !fatigue +. delta_t +. elast_l *. 0.1 +. press_x *. 0.01
        end;

        (* First CRITICAL forecast *)
        if !crit_t = -1 then
          (match Instability.estimate !state with
           | Instability.Critical -> crit_t := !state.timestep
           | _ -> ());

        prev_meta  := now_meta;
        prev_score := t.tension_score;
        w := Lnp.degrade !w degrade
      end
    end
  done;

  (* Regime classification *)
  let regime = match !rcls_t, !rupt_t with
    | -1, -1 -> 0   (* never recovered; no rupture (stuck in META or always CLOSED) *)
    | -1,  _ -> 1   (* immediate failure: no recovery, ruptured *)
    |  _,  r when r <> -1 -> 2   (* fragile: recovered at least once, then ruptured *)
    |  _,  _ -> 3   (* b_crit zone: recovered, no rupture in total_steps *)
  in

  { seed; boost; degrade; regime;
    n_meta    = !n_meta;
    rcls_t    = !rcls_t;
    rupt_t    = !rupt_t;
    crit_t    = !crit_t;
    max_tens  = !max_tens;
    max_rp    = !max_rp;
    min_sep   = !min_sep;
    min_elast = !min_elast;
    fatigue   = !fatigue;
  }

(* ── Helpers ─────────────────────────────────────────────────────────── *)

let regime_name = function
  | 0 -> "NEVER_META"
  | 1 -> "IMM_FAIL"
  | 2 -> "FRAGILE"
  | 3 -> "RECOVERED"
  | _ -> "UNKNOWN"

let finite f = f < 1e8

(* ── Main ────────────────────────────────────────────────────────────── *)

let () =
  let total_runs = Array.length degrades * Array.length boosts * n_seeds in

  (* ── CSV header to stdout ── *)
  print_string "seed,boost,degrade,regime,n_meta,rcls_t,rupt_t,crit_t,\
max_tens,max_rp,min_sep,min_elast,fatigue\n";

  (* ── Collect all results ── *)
  let all = Array.init (Array.length degrades) (fun di ->
    let degrade = degrades.(di) in
    Array.init (Array.length boosts) (fun bi ->
      let boost = boosts.(bi) in
      Array.init n_seeds (fun s ->
        let seed = s + 1 in
        let r = run_one ~degrade ~boost ~seed in
        (* Emit CSV row *)
        Printf.printf "%d,%.2f,%.3f,%d,%d,%d,%d,%d,%.4f,%.4f,%.4f,%.5f,%.4f\n"
          r.seed r.boost r.degrade r.regime r.n_meta
          r.rcls_t r.rupt_t r.crit_t
          r.max_tens r.max_rp r.min_sep
          (if finite r.min_elast then r.min_elast else 0.0)
          r.fatigue;
        r
      )
    )
  ) in

  (* ── Summary to stderr ── *)
  Printf.eprintf "\nStak-PSAL Stochastic Bifurcation Sweep\n";
  Printf.eprintf "  %d runs  (%d seeds × %d boost values × %d degrade rates)\n\n"
    total_runs n_seeds (Array.length boosts) (Array.length degrades);

  (* Regime distribution heatmap: rows = degrade, cols = boost *)
  Printf.eprintf "  Regime 3 fraction  (recovered, no rupture)  over %d seeds:\n" n_seeds;
  Printf.eprintf "  degrade\\boost   ";
  Array.iter (fun b -> Printf.eprintf " %4.2f" b) boosts;
  Printf.eprintf "\n  %s\n" (String.make (16 + 5 * Array.length boosts) '-');

  Array.iteri (fun di degrade ->
    Printf.eprintf "  %.3f           " degrade;
    Array.iteri (fun bi _ ->
      let seeds = all.(di).(bi) in
      let n3 = Array.fold_left (fun acc r -> if r.regime = 3 then acc + 1 else acc)
        0 seeds in
      Printf.eprintf "  %3d%%" n3
    ) boosts;
    Printf.eprintf "\n"
  ) degrades;
  Printf.eprintf "\n";

  (* b_crit per degradation rate: lowest boost where R3 > 50% *)
  Printf.eprintf "  Estimated b_crit (lowest boost where R3 > 50%%):\n";
  Array.iteri (fun di degrade ->
    let bc = ref None in
    Array.iteri (fun bi boost ->
      let seeds = all.(di).(bi) in
      let n3 = Array.fold_left (fun acc r -> if r.regime = 3 then acc + 1 else acc) 0 seeds in
      if !bc = None && n3 > n_seeds / 2 then
        bc := Some boost
    ) boosts;
    Printf.eprintf "    degrade=%.3f  b_crit≈%s\n" degrade
      (match !bc with None -> "≥0.60" | Some b -> Printf.sprintf "%.2f" b)
  ) degrades;
  Printf.eprintf "\n";

  (* Lead-time summary for regime 2 (fragile) across all (degrade, boost) *)
  let all_fragile = Array.to_list all
    |> List.concat_map Array.to_list
    |> List.concat_map Array.to_list
    |> List.filter (fun r -> r.regime = 2)
  in
  let with_lead = List.filter (fun r -> r.crit_t <> -1 && r.rupt_t <> -1) all_fragile in
  let leads = List.map (fun r -> r.rupt_t - r.crit_t) with_lead in
  let n_leads = List.length leads in
  if n_leads > 0 then begin
    let avg = List.fold_left ( + ) 0 leads |> float_of_int |> fun s -> s /. float_of_int n_leads in
    let mn  = List.fold_left min max_int leads in
    let mx  = List.fold_left max min_int leads in
    Printf.eprintf "  Lead time (CRITICAL → rupture) across all fragile runs:\n";
    Printf.eprintf "    n=%d  mean=%.1f  min=%d  max=%d step(s)\n\n" n_leads avg mn mx
  end;

  (* Fatigue comparison across regimes (aggregated over all degrade × boost) *)
  let all_flat = Array.to_list all
    |> List.concat_map Array.to_list
    |> List.concat_map Array.to_list
  in
  let avg_fatigue regime =
    let xs = List.filter_map (fun r ->
      if r.regime = regime && r.fatigue > 0.0 then Some r.fatigue else None
    ) all_flat in
    if xs = [] then 0.0
    else List.fold_left ( +. ) 0.0 xs /. float_of_int (List.length xs)
  in
  Printf.eprintf "  Mean cumulative fatigue F_n by regime:\n";
  Printf.eprintf "    Regime 1 (imm. failure) : %.3f\n"   (avg_fatigue 1);
  Printf.eprintf "    Regime 2 (fragile)      : %.3f\n"   (avg_fatigue 2);
  Printf.eprintf "    Regime 3 (recovered)    : %.3f\n\n" (avg_fatigue 3);

  Printf.eprintf "  Interpretation:\n";
  Printf.eprintf "  The fragile zone (Regime 2) demonstrates that local reclosure does\n";
  Printf.eprintf "  not imply global structural recovery.  Runs exhibit:\n";
  Printf.eprintf "    - Repeated transitions to CLOSED (apparent stability)\n";
  Printf.eprintf "    - Monotonic fatigue accumulation across episodes\n";
  Printf.eprintf "    - Final rupture after cumulative boundary erosion\n\n";
  Printf.eprintf "  This is structural fatigue dynamics: recovered ≠ restored.\n\n";

  Printf.eprintf "  Regime labels used in CSV:\n";
  for r = 0 to 3 do
    Printf.eprintf "    %d = %s\n" r (regime_name r)
  done;
  Printf.eprintf "\n"
