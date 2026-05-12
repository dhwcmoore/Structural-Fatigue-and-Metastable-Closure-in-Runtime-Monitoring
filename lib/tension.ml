(* Rolling Tension State — Tier 1 of the metabolic data pipeline.
   Individual cycle activations perish immediately; only their residue
   (Δ and gradient) is inherited by the tension accumulator. *)

open Types

let empty : tension_record = {
  tension_score        = 0.0;
  tension_slope        = 0.0;
  max_delta            = 0.0;
  oscillation_count    = 0;
  interval_widen_count = 0;
  boundary_revisions   = 0;
  recovery_elasticity  = 0.0;
  separation_ema       = 0.0;
  rupture_pressure     = 0.0;
}

(* Decay applied to tension each cycle (mimics metabolic clearance) *)
let decay = 0.92

let pressure_epsilon = 0.001   (* guard in 1/(d+ε) to prevent div-by-zero *)

(* update t dist gradient prev_gradient
   dist     : current projection distance M(x)–M(y)
   gradient : dist - prev_dist  (negative = converging, positive = recovering)
   prev_gradient : the gradient from the previous cycle                        *)
let update (t : tension_record)
           (dist : float)
           (gradient : float)
           (prev_gradient : float) : tension_record =
  (* Oscillation: gradient sign flips with non-trivial magnitude *)
  let oscillating =
    gradient *. prev_gradient < 0.0
    && abs_float gradient      > 0.004
    && abs_float prev_gradient > 0.004
  in
  (* Strain is highest when projections are closest to collapsing *)
  let strain    = max 0.0 (epsilon_meta -. dist) in
  let new_score = t.tension_score *. decay +. strain *. 0.6 in
  (* EMA of per-step tension increment: positive = strain accumulating,
     negative = enrichment winning.  α = 0.25 for moderate smoothing. *)
  let new_slope = 0.25 *. (new_score -. t.tension_score) +. 0.75 *. t.tension_slope in
  (* Separation EMA (α = 0.10): slow-moving; decline precedes collapse *)
  let new_sep   = 0.1 *. dist +. 0.9 *. t.separation_ema in
  (* Rupture pressure: rises as projections converge *)
  let new_rp    = 1.0 /. (dist +. pressure_epsilon) in
  { t with
    tension_score       = new_score;
    tension_slope       = new_slope;
    max_delta           = max t.max_delta dist;
    oscillation_count   =
      if oscillating then t.oscillation_count + 1 else t.oscillation_count;
    separation_ema      = new_sep;
    rupture_pressure    = new_rp;
    (* recovery_elasticity is set by Machine.step; carried through unchanged here *)
  }

let on_meta_review (t : tension_record) : tension_record =
  { t with interval_widen_count = t.interval_widen_count + 1 }

let on_boundary_revision (t : tension_record) : tension_record =
  { t with boundary_revisions = t.boundary_revisions + 1 }
