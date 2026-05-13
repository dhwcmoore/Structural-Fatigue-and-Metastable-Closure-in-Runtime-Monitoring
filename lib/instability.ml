(* Predictive instability estimation.
   Combines tension_slope, recovery_elasticity, and rupture_pressure into a
   three-level forecast that precedes actual collapse.

   The architecture shifts from reactive (rupture detected after collapse)
   to anticipatory (instability rising before collapse occurs):

     strain accumulation → instability forecast → intervention window

   Thresholds are calibrated from the b_crit = 0.30 boost-threshold sweep.
   At b_crit the failed-regime mean max_tension is 84.5× the recovered-regime
   mean, so the signal is strong even with conservative thresholds.

   Velocity-aware refinement (v2): the original fixed guard
   `steps_in_meta > 2` is replaced with a horizon-responsive policy.
   When the estimated rupture horizon is at most the admissibility lag,
   forced escalation bypasses the evidence-accumulation wait — there is no
   time to accumulate evidence if the horizon has already collapsed.        *)

open Types

type instability_level = Stable | Strained | Critical

let pp_level = function
  | Stable   -> "STABLE"
  | Strained -> "STRAINED"
  | Critical -> "CRITICAL"

(* ── Thresholds ──────────────────────────────────────────────────────── *)

let slope_warn        =  0.0005
let slope_critical    =  0.002
let elasticity_warn   =  0.005
let pressure_warn     =  5.0
let pressure_critical = 10.0

(* ── Rupture horizon estimation ──────────────────────────────────────── *)
(* Estimate h_t: steps remaining before d_t ≤ ε_collapse, using the
   one-step convergence rate observed in the previous gradient.

   Linear formula:  h_t = ⌈ (d_t − ε_c) / max(Δ, η) ⌉
     d_t = state.prev_dist  (distance set at end of the last step)
     Δ   = max(0, −prev_gradient)  (convergence per step; 0 if recovering)
     η   = horizon_eta  (from Types; regularisation against zero-convergence) *)

let horizon_estimate (state : machine_state) : int =
  let d   = state.prev_dist in
  let gap = d -. epsilon_collapse in
  if gap <= 0.0 then 0
  else
    let convergence = max 0.0 (-. state.prev_gradient) in
    let denom       = max convergence horizon_eta in
    int_of_float (Float.ceil (gap /. denom))

(* ── Dynamic wait policy ─────────────────────────────────────────────── *)
(* How many META_REVIEW steps to require before activating the elasticity
   signal, given the estimated horizon.  Reduces to admissibility_lag in
   the normal case (h > 3) and shrinks toward 0 as the horizon collapses. *)
let required_meta_steps (horizon : int) : int =
  if   horizon <= 1 then 0
  else if horizon <= 3 then 1
  else admissibility_lag

(* Horizon Debt (T3ν predicate): the remaining horizon does not exceed
   the admissibility lag.  The fixed-lag guard cannot activate in time.  *)
let horizon_debt (horizon : int) : bool =
  horizon <= admissibility_lag

(* ── Forecast ────────────────────────────────────────────────────────── *)

(* estimate state
   Returns the current instability level based on three signals:
     tension_slope       — dT/dt; positive = strain accumulating
     recovery_elasticity — E_r = ΔT_resolved/Δt; negative = enrichment losing
     rupture_pressure    — 1/(d+ε); rising = projections converging

   Forced escalation path (T1ν No Forced Lag Principle):
   When in META_REVIEW under horizon debt (h_t ≤ admissibility_lag), the
   monitor escalates to Critical immediately — bypassing the fixed wait —
   because there is no remaining time to accumulate evidence.

   Normal path: uses required_meta_steps(h_t) instead of the fixed 2 so
   the elasticity guard shrinks dynamically as the horizon tightens.      *)
let estimate (state : machine_state) : instability_level =
  let t        = state.tension in
  let h        = horizon_estimate state in
  let in_meta  = match state.status with MetaReview _ -> true | _ -> false in
  if in_meta && horizon_debt h then Critical
  else begin
    let req          = required_meta_steps h in
    let elast_active = state.steps_in_meta > req in
    let elast_low    = elast_active && t.recovery_elasticity < elasticity_warn in
    let critical =
      t.tension_slope    > slope_critical
      && elast_low
      && t.rupture_pressure > pressure_critical
    in
    let strained =
      t.tension_slope    > slope_warn
      || t.rupture_pressure > pressure_warn
      || elast_low
    in
    if critical then Critical
    else if strained then Strained
    else Stable
  end
