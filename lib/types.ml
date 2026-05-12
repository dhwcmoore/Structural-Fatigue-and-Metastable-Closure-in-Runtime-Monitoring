(* Core vocabulary for the STAK-PSAL architecture.
   Five foundational terms replace the representation-inference paradigm. *)

(* ── Admissibility thresholds ────────────────────────────────────────── *)

(* Projections closer than this are treated as "the same" in action space *)
let epsilon_collapse = 0.08

(* Distance below which the system enters heightened tension (META_REVIEW zone) *)
let epsilon_meta = 0.22

(* ── Action space ────────────────────────────────────────────────────── *)

type action = Maintain | EStop

let pp_action = function Maintain -> "Maintain" | EStop -> "EStop"

(* ── Tier 1: Rolling Tension State (every recurrent cycle) ──────────── *)
(* Individual cycle activations perish; only the residue is inherited.   *)

type tension_record = {
  tension_score        : float;   (* weighted sum of accumulated strain               *)
  tension_slope        : float;   (* EMA of per-step tension changes; + = worsening  *)
  max_delta            : float;   (* peak projection-distance seen so far             *)
  oscillation_count    : int;     (* gradient sign flips without convergence          *)
  interval_widen_count : int;     (* times the system expanded its search             *)
  boundary_revisions   : int;     (* context-enrichment corrections applied           *)
  recovery_elasticity  : float;   (* E_r = ΔT_resolved/Δt; + = enrichment winning   *)
  separation_ema       : float;   (* slow EMA of d_t; decline precedes collapse      *)
  rupture_pressure     : float;   (* 1/(d+ε); rising = projections converging        *)
}

(* ── Tier 2: Audit Snapshot (frozen on META_REVIEW transition) ───────── *)

type audit_snapshot = {
  reason            : string;        (* "OOD_SIGNAL" | "OSCILLATION" | "PROXIMITY_ALERT" *)
  boundary_snapshot : float array;   (* row-0 of W at the moment of entry                *)
  recent_deltas     : float list;    (* short buffer of recent projection distances       *)
  tension_at_entry  : tension_record;
  timestamp         : int;
}

(* ── Tier 3: Rupture Certificate (materialized on HARD_RUPTURE) ──────── *)

type witness_pair = {
  input_x      : float array;   (* the "Normal Pressure" 64D vector    *)
  input_y      : float array;   (* the "Critical Surge"  64D vector    *)
  projection_x : float array;   (* M(x) at moment of collapse          *)
  projection_y : float array;   (* M(y) at moment of collapse          *)
  action_x     : action;        (* Φ(x) — always Maintain in scenario  *)
  action_y     : action;        (* Φ(y) — always EStop   in scenario   *)
}

type phenomenological_residue = {
  curvature_divergence         : float;   (* sharpness of trajectory deviation   *)
  interval_widening_frequency  : int;     (* how often the system entered review *)
  unresolved_bifurcation_count : int;     (* snapshots that did not resolve      *)
  rupture_pressure             : float;   (* tension_score at collapse           *)
  inheritance_summary          : string;  (* compressed narrative of failure     *)
}

type rupture_certificate = {
  cert_status              : string;
  latent_projection        : float array;           (* collapsed midpoint M(x)≈M(y) *)
  witness                  : witness_pair;
  phenomenological_residue : phenomenological_residue;
  meta_state_status        : string;
}

(* ── System status ───────────────────────────────────────────────────── *)

type system_status =
  | Closed                            (* Δ_t < ε_meta and admissibility holds  *)
  | MetaReview of audit_snapshot      (* Δ_t > ε_meta or oscillatory behaviour *)
  | HardRupture of rupture_certificate (* M(x)=M(y) while Φ(x)≠Φ(y)           *)

let pp_status = function
  | Closed        -> "CLOSED"
  | MetaReview _  -> "META_REVIEW"
  | HardRupture _ -> "HARD_RUPTURE"

(* ── Machine state (threaded through every recurrent cycle) ─────────── *)

type machine_state = {
  status        : system_status;
  projection_x  : float array;
  projection_y  : float array;
  tension       : tension_record;
  timestep      : int;
  prev_dist     : float;              (* projection distance at t-1                    *)
  prev_gradient : float;              (* dist_{t-1} - dist_{t-2}; oscillation detect  *)
  snapshots     : audit_snapshot list;(* rolling audit trail (capped)                 *)
  tension_at_enrichment : float;      (* tension_score when enrichment last fired      *)
  steps_in_meta         : int;        (* steps elapsed in current META_REVIEW episode  *)
}
