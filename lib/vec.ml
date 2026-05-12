(* Vector and matrix primitives used throughout the LNP projection. *)

let dot a b =
  let n = Array.length a in
  let s = ref 0.0 in
  for i = 0 to n - 1 do s := !s +. a.(i) *. b.(i) done;
  !s

let dist a b =
  let n = Array.length a in
  let s = ref 0.0 in
  for i = 0 to n - 1 do
    let d = a.(i) -. b.(i) in
    s := !s +. d *. d
  done;
  sqrt !s

(* Matrix-vector product: mat is (k×D), vec is D → result is k *)
let matvec mat v = Array.map (fun row -> dot row v) mat

let sigmoid z = 1.0 /. (1.0 +. exp (-. z))

let rand_gauss () =
  (* Box-Muller transform *)
  let u1 = ref 0.0 in
  while !u1 = 0.0 do u1 := Random.float 1.0 done;
  let u2 = Random.float 1.0 in
  sqrt (-2.0 *. log !u1) *. cos (2.0 *. Float.pi *. u2)
