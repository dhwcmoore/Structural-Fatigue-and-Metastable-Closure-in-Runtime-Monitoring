(* Linear-Nonlinear Poisson (LNP) projection: 64D → 2D.
   Models neural dimensionality reduction via a learned weight matrix W
   followed by sigmoid activation. *)

open Vec

let dim_input  = 64
let dim_output = 2

(* Build the initial boundary-intact projection matrix W (2 × 64).
   Row 0 is strongly tuned to the pressure-critical dimension (dim 0).
   Row 1 encodes flow / temperature (dims 1–7). *)
let make_initial_w () =
  let w = Array.init dim_output (fun _ -> Array.make dim_input 0.0) in
  (* Row 0: pressure-critical — large initial weight on dim 0 *)
  w.(0).(0) <- 5.0;
  w.(0).(1) <- 0.10;
  w.(0).(2) <- 0.08;
  for j = 3 to dim_input - 1 do
    w.(0).(j) <- rand_gauss () *. 0.03
  done;
  (* Row 1: flow / temperature *)
  w.(1).(0) <- 0.10;
  for j = 1 to 7 do
    w.(1).(j) <- 0.50 +. rand_gauss () *. 0.10
  done;
  for j = 8 to dim_input - 1 do
    w.(1).(j) <- rand_gauss () *. 0.03
  done;
  w

(* LNP forward pass: z = W·x,  output = σ(z) *)
let project w x =
  let z = matvec w x in
  Array.map sigmoid z

(* Degrade the boundary by eroding the weight on dim 0 and injecting
   disorder into all other weights of row 0.
   rate ∈ (0, 1]: fraction of current weight lost in this step. *)
let degrade w rate =
  let w' = Array.map Array.copy w in
  w'.(0).(0) <- w.(0).(0) *. (1.0 -. rate);
  let noise = rate *. 0.25 in
  for j = 1 to dim_input - 1 do
    w'.(0).(j) <- w.(0).(j) +. rand_gauss () *. noise
  done;
  w'

(* Context enrichment: identify the dimension with the largest absolute
   gap between x and y and boost its weight in row 0 of W.
   This models the "reactivation of learned causal structures." *)
let context_enrich w x y boost =
  let best_j = ref 0 and best_gap = ref 0.0 in
  for j = 0 to dim_input - 1 do
    let g = abs_float (x.(j) -. y.(j)) in
    if g > !best_gap then begin best_gap := g; best_j := j end
  done;
  let w' = Array.map Array.copy w in
  w'.(0).(!best_j) <- w.(0).(!best_j) +. boost;
  w'
