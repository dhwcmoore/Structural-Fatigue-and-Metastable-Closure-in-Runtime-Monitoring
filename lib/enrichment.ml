(* Resolution elasticity and intervention viability.

   Given horizon debt (h_t ≤ admissibility_lag), determines which
   enrichment mechanisms can still restore timely admissibility before
   the rupture horizon closes.

   Architecture:
     horizon estimate → horizon debt detection → resolution elasticity ranking
     → opportunity cost tracking (rank loss since previous step)

   The base rupture horizon h_t remains pessimistic (no repair assumed).
   Resolution elasticity is a *second layer*: it asks which repair still
   fits inside the remaining horizon, and classifies each mechanism by the
   STAK-PSAL zone it restores:

     SurvivalViable   post_dt > ε_c        prevents hard rupture
     ReclosureViable  post_dt > ε_m        exits META_REVIEW zone
     DurableViable    ReclosureViable AND h_post > admissibility_lag  *)

open Types

(* ── Default mechanism catalogue ─────────────────────────────────────── *)
(* Conservative defaults calibrated to the four modalities in scenario.ml.
   Real deployments should supply domain-specific calibrated values.      *)

let default_mechanisms : enrichment_option list = [
  { kind          = MatrixBoost;
    latency_steps = 1;
    expected_gain = 0.040;
    confidence    = 0.90;
    cost          = 0.10;
    attack_risk   = 0.40;
    authenticated = false; };   (* not authenticated — adversarial risk too high *)
  { kind          = AdaptiveSampling;
    latency_steps = 1;
    expected_gain = 0.018;
    confidence    = 0.80;
    cost          = 0.20;
    attack_risk   = 0.10;
    authenticated = true; };
  { kind          = SensorEscalation;
    latency_steps = 3;
    expected_gain = 0.075;
    confidence    = 0.85;
    cost          = 0.40;
    attack_risk   = 0.10;
    authenticated = true; };
  { kind          = ModalityAugmentation;
    latency_steps = 2;
    expected_gain = 0.045;
    confidence    = 0.70;
    cost          = 0.50;
    attack_risk   = 0.20;
    authenticated = true; };
  { kind          = OperatorEscalation;
    latency_steps = 8;
    expected_gain = 0.200;
    confidence    = 0.95;
    cost          = 0.80;
    attack_risk   = 0.05;
    authenticated = true; };
]

(* ── Core computations ───────────────────────────────────────────────── *)

(* Expected separation after applying mechanism m.
   Accounts for degradation that continues during the latency window:
     post_dt = dt − latency × delta + confidence × expected_gain         *)
let post_intervention_dt (dt : float) (delta : float) (opt : enrichment_option) : float =
  dt
  -. (float_of_int opt.latency_steps *. delta)
  +. (opt.confidence *. opt.expected_gain)

(* Resolution elasticity E^m_res = q_m × gain_m − latency_m × delta.
   Positive → mechanism adds more admissibility margin than it concedes
   while waiting to take effect.                                          *)
let resolution_elasticity (delta : float) (opt : enrichment_option) : float =
  (opt.confidence *. opt.expected_gain)
  -. (float_of_int opt.latency_steps *. delta)

(* Estimate h_post: the rupture horizon after intervention, assuming
   convergence continues at rate delta through the post-intervention state. *)
let horizon_from_dist (dist : float) (delta : float) : int =
  let gap = dist -. epsilon_collapse in
  if gap <= 0.0 then 0
  else
    let denom = max delta horizon_eta in
    int_of_float (Float.ceil (gap /. denom))

(* ── Viability classification ─────────────────────────────────────────── *)

(* Lexicographic safety order:
     1. mechanism must arrive before horizon:  latency < horizon
     2. must be authenticated
     3. post_dt must exceed ε_c (survival)
     4. post_dt > ε_m → ReclosureViable
     5. ReclosureViable AND h_post > admissibility_lag → DurableViable   *)
let classify_viability
    ~(dt : float) ~(delta : float) ~(horizon : int)
    (opt : enrichment_option) : enrichment_viability =
  if not opt.authenticated then NotViable
  else if opt.latency_steps >= horizon then NotViable
  else
    let post_dt = post_intervention_dt dt delta opt in
    if post_dt <= epsilon_collapse then NotViable
    else if post_dt > epsilon_meta then
      let h_post = horizon_from_dist post_dt delta in
      if h_post > admissibility_lag then DurableViable
      else ReclosureViable
    else
      SurvivalViable

(* ── Ranked option ───────────────────────────────────────────────────── *)

type ranked_option = {
  option     : enrichment_option;
  viability  : enrichment_viability;
  elasticity : float;
  post_dt    : float;
}

let viability_rank = function
  | DurableViable   -> 3
  | ReclosureViable -> 2
  | SurvivalViable  -> 1
  | NotViable       -> 0

(* Lexicographic: viability desc → elasticity desc → latency asc *)
let compare_ranked (a : ranked_option) (b : ranked_option) : int =
  let rv = compare (viability_rank b.viability) (viability_rank a.viability) in
  if rv <> 0 then rv
  else
    let re = compare b.elasticity a.elasticity in
    if re <> 0 then re
    else compare a.option.latency_steps b.option.latency_steps

(* ── Viable options ──────────────────────────────────────────────────── *)

let viable_options
    ~(dt : float) ~(delta : float) ~(horizon : int)
    (mechanisms : enrichment_option list) : ranked_option list =
  mechanisms
  |> List.map (fun opt ->
       { option     = opt;
         viability  = classify_viability ~dt ~delta ~horizon opt;
         elasticity = resolution_elasticity delta opt;
         post_dt    = post_intervention_dt dt delta opt; })
  |> List.filter (fun ro -> ro.viability <> NotViable)
  |> List.sort compare_ranked

(* ── Opportunity Cost ─────────────────────────────────────────────────── *)

(* Explains why a mechanism does not achieve higher viability at the
   current step; invoked only when we know the rank has dropped.         *)
let viability_loss_reason ~(dt : float) ~(delta : float) ~(horizon : int)
    (opt : enrichment_option) : string =
  if not opt.authenticated then "authentication_not_cleared"
  else if opt.latency_steps >= horizon then
    Printf.sprintf "latency_%d_exceeds_horizon_%d" opt.latency_steps horizon
  else
    let post_dt = post_intervention_dt dt delta opt in
    if post_dt <= epsilon_collapse then
      Printf.sprintf "post_dt_%.4f_at_or_below_epsilon_c" post_dt
    else if post_dt <= epsilon_meta then
      Printf.sprintf "post_dt_%.4f_at_or_below_epsilon_m" post_dt
    else
      let h_post = horizon_from_dist post_dt delta in
      Printf.sprintf "h_post_%d_not_above_admissibility_lag" h_post

(* Human-readable description of the viability rank change.              *)
let viability_interpretation (prev_v : enrichment_viability)
    (curr_v : enrichment_viability) : string =
  Printf.sprintf
    "was certified %s under previous-step horizon; now %s — window closed"
    (pp_viability prev_v) (pp_viability curr_v)

type opportunity_cost_entry = {
  oce_mechanism            : enrichment_kind;
  oce_previous_step        : int;
  oce_current_step         : int;
  oce_previous_viability   : enrichment_viability;
  oce_current_viability    : enrichment_viability;
  oce_previous_post_dt     : float;
  oce_current_post_dt      : float;
  oce_prev_resolution_elast: float;
  oce_curr_resolution_elast: float;
  oce_lost_rank            : int;
  oce_reason               : string;
  oce_interpretation       : string;
}

(* OppCost_m(t) = max(0, V_m(t-1) − V_m(t)).
   Returns one entry per mechanism whose viability rank strictly fell.   *)
let compute_opportunity_costs
    ~(dt : float) ~(delta : float) ~(horizon : int)
    ~(prev_dt : float) ~(prev_delta : float) ~(prev_horizon : int)
    ~(prev_timestep : int) ~(timestep : int)
    (mechanisms : enrichment_option list) : opportunity_cost_entry list =
  List.filter_map (fun opt ->
    let prev_v = classify_viability
      ~dt:prev_dt ~delta:prev_delta ~horizon:prev_horizon opt in
    let curr_v = classify_viability ~dt ~delta ~horizon opt in
    let lost   = viability_rank prev_v - viability_rank curr_v in
    if lost <= 0 then None
    else
      Some {
        oce_mechanism             = opt.kind;
        oce_previous_step         = prev_timestep;
        oce_current_step          = timestep;
        oce_previous_viability    = prev_v;
        oce_current_viability     = curr_v;
        oce_previous_post_dt      = post_intervention_dt prev_dt prev_delta opt;
        oce_current_post_dt       = post_intervention_dt dt delta opt;
        oce_prev_resolution_elast = resolution_elasticity prev_delta opt;
        oce_curr_resolution_elast = resolution_elasticity delta opt;
        oce_lost_rank             = lost;
        oce_reason                = viability_loss_reason ~dt ~delta ~horizon opt;
        oce_interpretation        = viability_interpretation prev_v curr_v;
      }
  ) mechanisms

(* ── Horizon Debt Certificate ─────────────────────────────────────────── *)

type horizon_debt_certificate = {
  hdc_timestep         : int;
  hdc_dt               : float;
  hdc_delta            : float;
  hdc_horizon          : int;
  hdc_steps_in_meta    : int;
  hdc_required_steps   : int;
  hdc_viable           : ranked_option list;
  hdc_recommended      : enrichment_option option;
  hdc_opportunity_cost : opportunity_cost_entry list;
}

let build_hdc
    ~(dt : float) ~(delta : float) ~(horizon : int)
    ~(steps_in_meta : int) ~(required_steps : int)
    ~(timestep : int)
    ?(mechanisms = default_mechanisms)
    ?(prev_params : (float * float * int * int) option = None)
    () : horizon_debt_certificate =
  let viable = viable_options ~dt ~delta ~horizon mechanisms in
  let recommended = match viable with
    | []       -> None
    | best :: _ -> Some best.option
  in
  let opp_cost = match prev_params with
    | None -> []
    | Some (prev_dt, prev_delta, prev_horizon, prev_timestep) ->
      compute_opportunity_costs
        ~dt ~delta ~horizon
        ~prev_dt ~prev_delta ~prev_horizon
        ~prev_timestep ~timestep mechanisms
  in
  { hdc_timestep         = timestep;
    hdc_dt               = dt;
    hdc_delta            = delta;
    hdc_horizon          = horizon;
    hdc_steps_in_meta    = steps_in_meta;
    hdc_required_steps   = required_steps;
    hdc_viable           = viable;
    hdc_recommended      = recommended;
    hdc_opportunity_cost = opp_cost; }

(* ── JSON serialiser ─────────────────────────────────────────────────── *)

let viability_note = function
  | SurvivalViable  -> "prevents hard rupture; stays in META_REVIEW"
  | ReclosureViable -> "exits META_REVIEW; restores CLOSED"
  | DurableViable   -> "exits META_REVIEW; h_post > admissibility_lag"
  | NotViable       -> "infeasible"

let json_of_ranked (ro : ranked_option) : string =
  Printf.sprintf
    {|    {
      "mechanism": "%s",
      "latency_steps": %d,
      "expected_gain": %.4f,
      "confidence": %.4f,
      "resolution_elasticity": %.4f,
      "post_intervention_dt": %.4f,
      "viability": "%s",
      "note": "%s"
    }|}
    (pp_enrichment_kind ro.option.kind)
    ro.option.latency_steps
    ro.option.expected_gain
    ro.option.confidence
    ro.elasticity
    ro.post_dt
    (pp_viability ro.viability)
    (viability_note ro.viability)

let json_of_opp_cost (e : opportunity_cost_entry) : string =
  Printf.sprintf
    {|    {
      "mechanism": "%s",
      "previous_step": %d,
      "current_step": %d,
      "previous_viability": "%s",
      "current_viability": "%s",
      "lost_rank": %d,
      "previous_post_dt": %.4f,
      "current_post_dt": %.4f,
      "prev_resolution_elasticity": %.4f,
      "curr_resolution_elasticity": %.4f,
      "reason": "%s",
      "interpretation": "%s"
    }|}
    (pp_enrichment_kind e.oce_mechanism)
    e.oce_previous_step
    e.oce_current_step
    (pp_viability e.oce_previous_viability)
    (pp_viability e.oce_current_viability)
    e.oce_lost_rank
    e.oce_previous_post_dt
    e.oce_current_post_dt
    e.oce_prev_resolution_elast
    e.oce_curr_resolution_elast
    e.oce_reason
    e.oce_interpretation

let to_json (cert : horizon_debt_certificate) : string =
  let rec_str = match cert.hdc_recommended with
    | None   -> "none"
    | Some o -> pp_enrichment_kind o.kind
  in
  let viable_block =
    match cert.hdc_viable with
    | [] -> ""
    | vs -> "\n" ^ String.concat ",\n" (List.map json_of_ranked vs) ^ "\n  "
  in
  let opp_block =
    match cert.hdc_opportunity_cost with
    | [] -> ""
    | es -> "\n" ^ String.concat ",\n" (List.map json_of_opp_cost es) ^ "\n  "
  in
  Printf.sprintf
    {|{
  "certificate_type": "horizon_debt",
  "status": "critical",
  "timestep": %d,
  "admissibility_lag": %d,
  "dt": %.4f,
  "epsilon_c": %.2f,
  "epsilon_m": %.2f,
  "delta_estimate": %.4f,
  "horizon_estimate": %d,
  "steps_in_meta": %d,
  "required_meta_steps": %d,
  "safe_to_wait": false,
  "viable_enrichments": [%s],
  "recommended_action": "%s",
  "opportunity_cost": [%s]
}|}
    cert.hdc_timestep
    admissibility_lag
    cert.hdc_dt
    epsilon_collapse
    epsilon_meta
    cert.hdc_delta
    cert.hdc_horizon
    cert.hdc_steps_in_meta
    cert.hdc_required_steps
    viable_block
    rec_str
    opp_block
