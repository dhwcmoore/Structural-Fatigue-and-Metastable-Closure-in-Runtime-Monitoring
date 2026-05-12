(* Predictive instability estimation.
   Combines tension_slope, recovery_elasticity, and rupture_pressure into a
   three-level forecast that precedes actual collapse.

   The architecture shifts from reactive (rupture detected after collapse)
   to anticipatory (instability rising before collapse occurs):

     strain accumulation → instability forecast → intervention window

   Thresholds are calibrated from the b_crit = 0.30 boost-threshold sweep.
   At b_crit the failed-regime mean max_tension is 84.5× the recovered-regime
   mean, so the signal is strong even with conservative thresholds.            *)

open Types

type instability_level = Stable | Strained | Critical

let pp_level = function
  | Stable   -> "STABLE"
  | Strained -> "STRAINED"
  | Critical -> "CRITICAL"

(* ── Thresholds ──────────────────────────────────────────────────────── *)

(* Any positive tension slope: strain beginning to accumulate *)
let slope_warn        =  0.0005
(* Rapid accumulation: tension rising faster than decay can clear *)
let slope_critical    =  0.002
(* Enrichment barely resolving anything per step *)
let elasticity_warn   =  0.005
(* d < ~0.20: entering ε_meta zone *)
let pressure_warn     =  5.0
(* d < ~0.10: approaching ε_collapse *)
let pressure_critical = 10.0

(* ── Forecast ────────────────────────────────────────────────────────── *)

(* estimate state
   Returns the current instability level based on three signals:
     tension_slope       — dT/dt; positive = strain accumulating
     recovery_elasticity — E_r = ΔT_resolved/Δt; negative = enrichment losing
     rupture_pressure    — 1/(d+ε); rising = projections converging

   The elasticity signal is only activated after the system has been in
   META_REVIEW for 2+ steps — allowing enrichment time to act before
   its effect can be measured.                                            *)
let estimate (state : machine_state) : instability_level =
  let t = state.tension in
  let elast_active = state.steps_in_meta > 2 in
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
