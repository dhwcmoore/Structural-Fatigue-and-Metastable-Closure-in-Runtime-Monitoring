(* Admissibility: M(x) = M(y)  →  Φ(x) = Φ(y).
   This module implements the safety function Φ and the three-way check
   that drives the state machine. *)

open Types
open Vec

(* ── Safety function Φ: 64D → Action ────────────────────────────────── *)
(* Driven by the pressure-critical dimension 0 of the raw state.        *)
let phi (x : float array) : action =
  if x.(0) > 0.70 then EStop else Maintain

(* ── Admissibility check ─────────────────────────────────────────────── *)

type check_result =
  | Admissible_  of float array * float array * float   (* mx, my, dist *)
  | Drifting_    of float array * float array * float
  | HardRupture_ of float array * float array * action * action * float

(* check proj_fn x y classifies the current system status:
     - HardRupture_ : projections have collapsed AND safety actions diverge
     - Drifting_    : projections within the META_REVIEW tension zone
     - Admissible_  : well-separated; boundary discipline intact           *)
let check (proj_fn : float array -> float array)
           (x : float array) (y : float array) : check_result =
  let mx = proj_fn x and my = proj_fn y in
  let d  = dist mx my in
  let ax = phi x       and ay = phi y in
  if d < epsilon_collapse && ax <> ay then
    HardRupture_ (mx, my, ax, ay, d)
  else if d < epsilon_meta then
    Drifting_ (mx, my, d)
  else
    Admissible_ (mx, my, d)
