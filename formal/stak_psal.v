(* ═══════════════════════════════════════════════════════════════════════
   STAK-PSAL: Boundary Closure as Stabilized Admissibility
   Formal Specification in Coq / Rocq

   This file provides machine-checked proofs for the core admissibility
   invariant that the OCaml toy demonstrator is designed to preserve.
   A "Hard Rupture" is formalised as a witness that this invariant has
   been violated.

   Dependency: Require Import Classical  (excluded middle for B-cases).
   ═══════════════════════════════════════════════════════════════════════ *)

Require Import Classical.

(* ── §1  Vocabulary ───────────────────────────────────────────────────── *)

(** Raw process-states (64-dimensional sensor readings, treated abstractly). *)
Variable State      : Type.

(** The action space after dimensionality reduction (2D in the demonstrator). *)
Variable Projection : Type.

(** Safety actions required at the control interface. *)
Inductive Action := Maintain | EStop.

(** The Observation Map  M : State → Projection
    The reductive operator that projects high-dimensional process-states
    into observable, "action-ready" states.                                *)
Variable M : State -> Projection.

(** The Safety Function  Φ : State → Action
    The ground-truth control requirement; never changes with time.         *)
Variable Phi : State -> Action.

(* ── §2  Core Definitions ─────────────────────────────────────────────── *)

(** Admissibility
    The reduction M must never collapse two states whose required safety
    actions differ.
       M(x) = M(y)  →  Φ(x) = Φ(y)                                       *)
Definition Admissible : Prop :=
  forall x y : State, M x = M y -> Phi x = Phi y.

(** Rupture
    Dual of Admissibility: the existence of a witness pair (x, y) whose
    projections have collapsed yet whose safety actions diverge.            *)
Definition Rupture : Prop :=
  exists x y : State, M x = M y /\ Phi x <> Phi y.

(** Closure
    Admissibility is maintained ("closed") under the recurrent feedback
    operator R.  We characterise stability as fixed-point convergence.     *)
Variable R_op : Projection -> Projection.

Definition Closure : Prop :=
  Admissible /\ forall p : Projection, R_op p = p.

(* ── §3  Mutual Exclusion ─────────────────────────────────────────────── *)

(** A system cannot simultaneously be Admissible and Ruptured.             *)
Theorem admissible_not_ruptured :
  Admissible -> ~ Rupture.
Proof.
  intros Hadm [x [y [Hproj Hact]]].
  exact (Hact (Hadm x y Hproj)).
Qed.

(** Equivalently, Rupture implies the breakdown of Admissibility.          *)
Corollary ruptured_implies_not_admissible :
  Rupture -> ~ Admissible.
Proof.
  intros Hrup Hadm.
  exact (admissible_not_ruptured Hadm Hrup).
Qed.

(* ── §4  Boundary-Governed Admissibility ─────────────────────────────── *)

(** A Boundary B partitions State into safety-distinct regions.
    BoundaryPreserved: if B separates x from y in State-space,
    then M must also separate them in Projection-space.                    *)
Variable B : State -> Prop.

Definition BoundaryPreserved : Prop :=
  forall x y : State, (B x /\ ~ B y) -> M x <> M y.

(** The safety function respects B:
      B-states require EStop; non-B-states require Maintain.               *)
Hypothesis phi_B    : forall x : State,   B x -> Phi x = EStop.
Hypothesis phi_notB : forall x : State, ~ B x -> Phi x = Maintain.

(** Main result
    If the Boundary is preserved by M, then M is Admissible.

    Proof sketch:
      Case x ∈ B: Φ(x) = EStop.  If y ∉ B, BoundaryPreserved gives
        M(x) ≠ M(y), contradicting the hypothesis.  So y ∈ B, Φ(y) = EStop.
      Case x ∉ B: Φ(x) = Maintain.  If y ∈ B, BoundaryPreserved (with
        roles reversed) gives M(y) ≠ M(x), contradiction.  So y ∉ B,
        Φ(y) = Maintain.                                                   *)
Theorem boundary_preservation_implies_admissibility :
  BoundaryPreserved -> Admissible.
Proof.
  intros Hbp x y Hmxy.
  destruct (classic (B x)) as [HBx | HnBx].

  (* Case 1: x ∈ B  →  Φ(x) = EStop *)
  - rewrite (phi_B x HBx).
    destruct (classic (B y)) as [HBy | HnBy].
    + exact (eq_sym (phi_B y HBy)).
    + (* y ∉ B: BoundaryPreserved gives M(x) ≠ M(y); contradicts Hmxy *)
      exfalso.
      exact (Hbp x y (conj HBx HnBy) Hmxy).

  (* Case 2: x ∉ B  →  Φ(x) = Maintain *)
  - rewrite (phi_notB x HnBx).
    destruct (classic (B y)) as [HBy | HnBy].
    + (* y ∈ B, x ∉ B: symmetric application of BoundaryPreserved *)
      exfalso.
      exact (Hbp y x (conj HBy HnBx) (eq_sym Hmxy)).
    + exact (eq_sym (phi_notB y HnBy)).
Qed.

(* ── §5  Rupture Certificate ─────────────────────────────────────────── *)

(** A Hard Rupture Certificate is a dependent record containing the
    witness pair together with machine-checked proofs of:
      (i)  the projections have collapsed:    M(x) = M(y)
      (ii) the actions diverge:               Φ(x) ≠ Φ(y)               *)
Record RuptureCertificate : Type := mkCert {
  cert_x    : State;
  cert_y    : State;
  cert_proj : M cert_x = M cert_y;
  cert_div  : Phi cert_x <> Phi cert_y;
}.

(** A certificate is sufficient evidence of Rupture.                       *)
Lemma certificate_implies_rupture (cert : RuptureCertificate) : Rupture.
Proof.
  unfold Rupture.
  exists (cert_x cert), (cert_y cert).
  exact (conj (cert_proj cert) (cert_div cert)).
Qed.

(** A valid certificate refutes Admissibility.                             *)
Theorem certificate_refutes_admissibility (cert : RuptureCertificate)
    : ~ Admissible.
Proof.
  apply ruptured_implies_not_admissible.
  exact (certificate_implies_rupture cert).
Qed.

(* ── §6  Closure Excludes Certificates ──────────────────────────────── *)

(** Under Closure, no Rupture Certificate can be constructed.             *)
Theorem closure_excludes_rupture :
  Closure -> forall (cert : RuptureCertificate), False.
Proof.
  intros [Hadm _] cert.
  exact (certificate_refutes_admissibility cert Hadm).
Qed.

(** Reformulation: Closure is strictly stronger than "no witness exists." *)
Corollary closure_implies_no_rupture : Closure -> ~ Rupture.
Proof.
  intros [Hadm _].
  exact (admissible_not_ruptured Hadm).
Qed.

(* ── §7  Boundary Erosion and the Path to Rupture ───────────────────── *)

(** If BoundaryPreserved fails, Rupture becomes possible — the system
    transitions from CLOSED to the danger zone.

    Here we show the contrapositive: Rupture implies the boundary
    has been violated (BoundaryPreserved no longer holds).                *)
Theorem rupture_implies_boundary_violated :
  Rupture -> ~ BoundaryPreserved.
Proof.
  intros Hrup Hbp.
  apply (ruptured_implies_not_admissible Hrup).
  exact (boundary_preservation_implies_admissibility Hbp).
Qed.

(* ═══════════════════════════════════════════════════════════════════════
   End of STAK-PSAL formal specification.
   The OCaml toy demonstrator in bin/main.ml exercises these invariants
   dynamically, emitting a JSON Rupture Certificate when §5 is witnessed.
   ═══════════════════════════════════════════════════════════════════════ *)
