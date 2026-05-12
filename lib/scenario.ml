(* Synthetic four-modality industrial process benchmark.
   Defines structured 64-dimensional inputs modelling a process-monitoring
   system with four sensor classes:

     dims  0-15 : pressure   (safety-critical; primary boundary dim = 0)
     dims 16-31 : temperature
     dims 32-47 : flow rate
     dims 48-63 : vibration

   The safety function Φ is driven by dim 0 (pressure), consistent with
   the base STAK-PSAL scenario.  The additional modalities provide
   correlated evidence that degrades at modality-specific rates.

   Multi-modal degradation:
     Pressure boundary erodes fastest (highest operational stress).
     Vibration second (mechanical fatigue).
     Temperature third.
     Flow slowest (hydraulic inertia).                                    *)

open Vec

let n_modalities    = 4
let dims_per_mod    = 16      (* 4 × 16 = 64 total *)

(* ── Modality offsets ────────────────────────────────────────────────── *)
let pressure_base    = 0
let temperature_base = 16
let flow_base        = 32
let vibration_base   = 48

(* ── Default multi-modal degradation rates ───────────────────────────── *)
type degrade_rates = {
  pressure    : float;   (* fastest — safety-critical boundary *)
  temperature : float;
  flow        : float;   (* slowest — hydraulic inertia        *)
  vibration   : float;
}

let default_rates = {
  pressure    = 0.020;
  temperature = 0.012;
  flow        = 0.007;
  vibration   = 0.015;
}

(* ── State generation ────────────────────────────────────────────────── *)

(* Normal operating state x:
   Pressure low (dim 0 = 0.15), temperature ambient, flow nominal,
   vibration quiet.  Other dims: modality-typical base ± small noise. *)
let make_normal_state ?(noise = 0.04) () =
  Array.init 64 (fun i ->
    let base =
      if   i = pressure_base    then 0.15   (* primary pressure: normal     *)
      else if i < temperature_base then 0.40 +. rand_gauss () *. noise
      else if i = temperature_base then 0.20  (* primary temperature: ambient *)
      else if i < flow_base then 0.40 +. rand_gauss () *. noise
      else if i = flow_base       then 0.55   (* primary flow: nominal        *)
      else if i < vibration_base then 0.50 +. rand_gauss () *. noise
      else if i = vibration_base  then 0.10   (* primary vibration: quiet     *)
      else 0.45 +. rand_gauss () *. noise
    in
    (* The anchor dims (primary of each modality) are exact; others noisy *)
    if i = pressure_base || i = temperature_base
       || i = flow_base || i = vibration_base
    then base
    else 0.50 +. rand_gauss () *. noise)

(* Critical operating state y:
   Pressure surge (dim 0 = 0.88), temperature elevated, flow reduced
   (blockage), vibration elevated (mechanical stress). *)
let make_critical_state ?(noise = 0.04) () =
  Array.init 64 (fun i ->
    if   i = pressure_base    then 0.88   (* pressure surge: critical     *)
    else if i < temperature_base then 0.55 +. rand_gauss () *. noise
    else if i = temperature_base then 0.72 (* temperature elevated         *)
    else if i < flow_base then 0.55 +. rand_gauss () *. noise
    else if i = flow_base       then 0.22 (* flow reduced (blockage)      *)
    else if i < vibration_base then 0.50 +. rand_gauss () *. noise
    else if i = vibration_base  then 0.76 (* vibration elevated           *)
    else 0.55 +. rand_gauss () *. noise)

(* ── W initialization: multi-modal boundary matrix ───────────────────── *)
(* Row 0 encodes the pressure-critical boundary (high weight on dim 0)
   plus supporting weights on the correlated modalities.
   Row 1 encodes the secondary flow/temperature projection.              *)
let make_initial_w () =
  let w = Array.init 2 (fun _ -> Array.make 64 0.0) in

  (* Row 0: pressure boundary — primary + corroborating modalities *)
  w.(0).(pressure_base)    <- 5.0;    (* safety-critical anchor *)
  w.(0).(pressure_base+1)  <- 0.40;
  w.(0).(pressure_base+2)  <- 0.30;
  for j = pressure_base+3 to temperature_base-1 do
    w.(0).(j) <- rand_gauss () *. 0.05
  done;
  w.(0).(temperature_base) <- 0.80;   (* temperature corroborates pressure   *)
  w.(0).(temperature_base+1) <- 0.20;
  for j = temperature_base+2 to flow_base-1 do
    w.(0).(j) <- rand_gauss () *. 0.04
  done;
  for j = flow_base to vibration_base-1 do
    w.(0).(j) <- rand_gauss () *. 0.03
  done;
  w.(0).(vibration_base)   <- 0.60;   (* vibration: structural confirmation  *)
  w.(0).(vibration_base+1) <- 0.15;
  for j = vibration_base+2 to 63 do
    w.(0).(j) <- rand_gauss () *. 0.04
  done;

  (* Row 1: flow/temperature secondary projection *)
  w.(1).(flow_base)        <- 2.0;
  w.(1).(flow_base+1)      <- 0.50;
  w.(1).(flow_base+2)      <- 0.30;
  for j = flow_base+3 to vibration_base-1 do
    w.(1).(j) <- 0.20 +. rand_gauss () *. 0.08
  done;
  w.(1).(temperature_base) <- 0.80;
  for j = temperature_base+1 to flow_base-1 do
    w.(1).(j) <- 0.30 +. rand_gauss () *. 0.06
  done;
  for j = 0 to pressure_base+5 do
    w.(1).(j) <- rand_gauss () *. 0.02
  done;
  w

(* ── Multi-modal degradation ─────────────────────────────────────────── *)
(* Erodes each modality block of W at its own rate.  Within each block,
   the primary dim (j = base) decays proportionally; remaining dims
   receive proportional noise injection (like the base degrade). *)
let degrade_multimodal w (rates : degrade_rates) =
  let w' = Array.map Array.copy w in
  let modality_rate j =
    let m = j / dims_per_mod in
    match m with
    | 0 -> rates.pressure
    | 1 -> rates.temperature
    | 2 -> rates.flow
    | _ -> rates.vibration
  in
  for row = 0 to 1 do
    for j = 0 to 63 do
      let r = modality_rate j in
      w'.(row).(j) <- w.(row).(j) *. (1.0 -. r)
                    +. rand_gauss () *. (r *. 0.20)
    done
  done;
  w'

(* ── Scenario description ─────────────────────────────────────────────── *)
let pp_rates r =
  Printf.sprintf
    "pressure=%.3f  temperature=%.3f  flow=%.3f  vibration=%.3f"
    r.pressure r.temperature r.flow r.vibration

let _ = n_modalities  (* suppress unused warning *)
