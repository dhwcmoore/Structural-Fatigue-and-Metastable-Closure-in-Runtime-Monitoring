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
Require Import Lia.

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

(* ── §8  Velocity-Aware Admissibility ────────────────────────────────── *)
(* The strict invariants (T1)–(T5) govern whether the safety distinction
   is structurally visible:  can M(x) = M(y) while Φ(x) ≠ Φ(y)?

   This section adds a second invariant layer (T1ν)–(T5ν) governing
   whether the distinction is still actionable:  can the monitor respond
   before the rupture horizon closes?

   The formal distinction:
     Admissibility     → the distinction is structurally visible.
     TimelyAdmissible  → the distinction is visible AND actionable.

   The key concept is Admissibility Lag (L_t): the number of META_REVIEW
   steps the monitor requires before its critical signal activates.  When
   the estimated Rupture Horizon (h_t) is no greater than L_t, the monitor
   cannot accumulate the required evidence in the remaining time.  This
   state is called Horizon Debt.                                          *)

Section VelocityAwareAdmissibility.

(** Abstract monitor snapshot.                                            *)
Variable MonState : Type.

(** The monitor is in META_REVIEW at state [s].                          *)
Variable in_meta_review : MonState -> Prop.

(** Steps spent in META_REVIEW at state [s].                             *)
Variable steps_in_meta : MonState -> nat.

(** Estimated remaining rupture horizon: steps until d_t ≤ ε_collapse.  *)
Variable remaining_horizon : MonState -> nat.

(** Admissibility lag: fixed evidence-accumulation delay.
    In the current OCaml implementation this constant is 2,
    corresponding to the guard `steps_in_meta > 2`.                      *)
Variable admissibility_lag : nat.

(** (Def) Horizon Debt.
    The remaining horizon does not exceed the lag.  Under Horizon Debt
    the fixed-lag guard cannot activate before rupture arrives.           *)
Definition HorizonDebt (s : MonState) : Prop :=
  remaining_horizon s <= admissibility_lag.

(** (Def) Safe to wait.
    The remaining horizon strictly exceeds the lag.                       *)
Definition SafeToWait (s : MonState) : Prop :=
  remaining_horizon s > admissibility_lag.

(** (Def) Timely Admissibility.
    Strict admissibility holds AND the horizon gives the monitor time to act.
      Admissible     → the distinction is structurally visible.
      SafeToWait s   → the distinction is still actionable.              *)
Definition TimelyAdmissibleState (s : MonState) : Prop :=
  Admissible /\ SafeToWait s.

(** (Def) Fixed-lag guard.
    The original implementation condition gating the critical signal.     *)
Definition FixedLagGuard (s : MonState) : Prop :=
  steps_in_meta s > admissibility_lag.

(** (T1ν) No Forced Lag Principle.
    In META_REVIEW under Horizon Debt, passive evidence accumulation is
    unsafe: there is no time to wait for the lag to expire.

    This is the formal counterpart of the NDT argument: just as a neural
    decision system cannot begin evidence accumulation before its
    non-decision time has elapsed, the monitor cannot classify Critical
    via the fixed-lag path when the horizon has already collapsed.        *)
Theorem no_forced_lag_under_short_horizon :
  forall s : MonState,
    in_meta_review s ->
    HorizonDebt s ->
    ~ SafeToWait s.
Proof.
  intros s _ Hdebt Hwait.
  unfold HorizonDebt, SafeToWait in *.
  lia.
Qed.

(** (T2ν) Timely admissibility requires horizon surplus.
    A system is timely admissible only when the remaining horizon
    strictly exceeds the admissibility lag.                               *)
Theorem timely_admissibility_requires_horizon :
  forall s : MonState,
    TimelyAdmissibleState s ->
    remaining_horizon s > admissibility_lag.
Proof.
  intros s [_ Hwait].
  exact Hwait.
Qed.

(** (T3ν) Fixed-lag guard is inactive under early Horizon Debt.
    When the monitor enters META_REVIEW under Horizon Debt and has
    accumulated at most [admissibility_lag] steps, the fixed-lag guard
    is False — it requires more steps than the horizon provides.

    Formally: `steps_in_meta > admissibility_lag` is sound only under
    the side condition `remaining_horizon > admissibility_lag`.          *)
Theorem fixed_lag_inactive_under_horizon_debt :
  forall s : MonState,
    HorizonDebt s ->
    steps_in_meta s <= admissibility_lag ->
    ~ FixedLagGuard s.
Proof.
  intros s Hdebt Hsteps.
  unfold FixedLagGuard.
  lia.
Qed.

(** (T4ν) Closure under Horizon Debt does not imply safe waiting.
    A system may satisfy Closure (no present rupture certificate exists)
    while simultaneously being unsafe to leave in passive monitoring.

    Velocity-aware counterpart of the central thesis:
      "Operational closure does not imply timely admissibility."
    Or more sharply:
      "Closure excludes present rupture but does not exclude
       horizon collapse."                                                 *)
Theorem closure_does_not_imply_safe_waiting :
  forall s : MonState,
    Closure ->
    HorizonDebt s ->
    ~ SafeToWait s.
Proof.
  intros s _ Hdebt Hwait.
  unfold HorizonDebt, SafeToWait in *.
  lia.
Qed.

(** (T5ν) Horizon Debt at META_REVIEW entry.
    A system that enters META_REVIEW for the first time (steps_in_meta = 1)
    under Horizon Debt cannot satisfy the fixed-lag guard.
    Corollary of (T3ν) at the initial step.                              *)
Corollary horizon_debt_at_meta_entry :
  forall s : MonState,
    HorizonDebt s ->
    steps_in_meta s = 1 ->
    ~ FixedLagGuard s.
Proof.
  intros s Hdebt Hsteps.
  apply fixed_lag_inactive_under_horizon_debt.
  - exact Hdebt.
  - lia.
Qed.

(** Horizon Debt Certificate.
    A record witnessing that Horizon Debt has been detected.

    Unlike a Rupture Certificate — which witnesses a distinction that has
    already collapsed (M(x) = M(y) while Φ(x) ≠ Φ(y)) — a Horizon Debt
    Certificate witnesses a distinction that is structurally visible but
    has become operationally too late to act upon:
      Admissible holds  →  the distinction is still visible.
      HorizonDebt holds →  the monitor cannot respond in time.

    This is a new class of failure certificate between MetaReview and
    HardRupture in the monitor's escalation hierarchy.                    *)
Record HorizonDebtCertificate : Type := mkHDCert {
  hdc_state   : MonState;
  hdc_in_meta : in_meta_review hdc_state;
  hdc_debt    : HorizonDebt hdc_state;
}.

(** A Horizon Debt Certificate witnesses the absence of safe waiting.    *)
Theorem hdc_implies_not_safe_to_wait :
  forall cert : HorizonDebtCertificate,
    ~ SafeToWait (hdc_state cert).
Proof.
  intros cert.
  exact (no_forced_lag_under_short_horizon
           (hdc_state cert)
           (hdc_in_meta cert)
           (hdc_debt cert)).
Qed.

End VelocityAwareAdmissibility.

(* ── §9  Resolution Elasticity ───────────────────────────────────────── *)
(* §8 establishes when the monitor must escalate (Horizon Debt).
   §9 answers what escalation is still possible: which enrichment
   mechanisms can restore Timely Admissibility before the horizon closes.

   The key concept is Resolution Elasticity:
     E^m_res = q_m × gain_m − latency_m × delta
   Positive resolution elasticity means the mechanism adds more
   admissibility margin than is lost while waiting for it to arrive.

   Architecture:
     horizon estimate → horizon debt detection → resolution elasticity ranking
                                                          (§9)               *)

Section ResolutionElasticity.

(** Abstract monitor state.                                               *)
Variable MonState  : Type.

(** Abstract enrichment mechanism.                                        *)
Variable Mechanism : Type.

(** Estimated remaining rupture horizon at [s].                           *)
Variable remaining_horizon : MonState -> nat.

(** The monitor's admissibility lag.                                      *)
Variable admissibility_lag : nat.

(** The monitor is in META_REVIEW at state [s].                          *)
Variable in_meta_review : MonState -> Prop.

(** Horizon Debt: the remaining horizon does not exceed the lag.
    (Re-stated locally so this Section is self-contained.)                *)
Definition HorizonDebt' (s : MonState) : Prop :=
  remaining_horizon s <= admissibility_lag.

(** The mechanism must arrive within [latency] steps.                     *)
Variable latency : Mechanism -> nat.

(** The mechanism has positive resolution elasticity at state [s]:
    its expected gain exceeds the margin lost during its latency window. *)
Variable resolution_elasticity_positive : MonState -> Mechanism -> Prop.

(** The mechanism is authenticated in the deployment context.             *)
Variable authenticated : Mechanism -> Prop.

(** Viability: the mechanism can still restore timely admissibility.
    All three conditions must hold:
      (i)   it arrives before the horizon closes
      (ii)  it is authenticated
      (iii) its resolution elasticity is positive                         *)
Definition viable_enrichment (s : MonState) (m : Mechanism) : Prop :=
  latency m < remaining_horizon s /\
  authenticated m /\
  resolution_elasticity_positive s m.

(** The estimated remaining horizon after the mechanism is applied.       *)
Variable remaining_horizon_after : MonState -> Mechanism -> nat.

(** Certified gain contract: a viable mechanism raises h_post above lag.
    This is an axiom — Coq cannot verify that a real sensor will work.
    It can only prove: IF the mechanism satisfies its certified contract,
    THEN timely admissibility is restored.                                *)
Axiom gain_sound :
  forall s m,
    viable_enrichment s m ->
    remaining_horizon_after s m > admissibility_lag.

(** Timely admissibility of the post-intervention state.                  *)
Definition TimelyAdmissibleAfter (s : MonState) (m : Mechanism) : Prop :=
  remaining_horizon_after s m > admissibility_lag.

(** (T1§9) Sound viable mechanism restores timely admissibility.
    The key theorem: IF the mechanism satisfies its gain contract,
    THEN applying it under horizon debt restores timeliness.              *)
Theorem sound_viable_enrichment_restores_timeliness :
  forall s m,
    HorizonDebt' s ->
    viable_enrichment s m ->
    TimelyAdmissibleAfter s m.
Proof.
  intros s m _ Hvie.
  unfold TimelyAdmissibleAfter.
  exact (gain_sound s m Hvie).
Qed.

(** (T2§9) Horizon debt does not preclude recovery.
    Under Horizon Debt, if any viable mechanism exists, timely
    admissibility can be restored.                                        *)
Theorem horizon_debt_admits_recovery :
  forall s,
    HorizonDebt' s ->
    (exists m, viable_enrichment s m) ->
    exists m, TimelyAdmissibleAfter s m.
Proof.
  intros s Hdebt [m Hvie].
  exists m.
  exact (sound_viable_enrichment_restores_timeliness s m Hdebt Hvie).
Qed.

(** (T3§9) Triage is required under Horizon Debt.
    In META_REVIEW under Horizon Debt, if a viable mechanism exists,
    the monitor must evaluate and apply it rather than waiting passively.

    This is the operational complement of (T1ν): passive waiting is
    unsafe (T1ν), but active triage can restore safety (T3§9).          *)
Theorem triage_required_under_horizon_debt :
  forall s,
    in_meta_review s ->
    HorizonDebt' s ->
    (exists m, viable_enrichment s m) ->
    exists m, TimelyAdmissibleAfter s m.
Proof.
  intros s _ Hdebt [m Hvie].
  exists m.
  exact (sound_viable_enrichment_restores_timeliness s m Hdebt Hvie).
Qed.

(** (T4§9) No viable mechanism blocks certified enrichment-mediated recovery.

    The honest statement: without a converse to gain_sound we cannot prove
    that rupture is unavoidable when no mechanism is viable — doing so would
    require knowing that every non-viable mechanism also fails to restore
    timeliness, i.e., the converse (TimelyAdmissibleAfter → viable), which
    gain_sound does not supply.

    What Coq CAN prove:  if no mechanism is viable, then no mechanism can
    be **certified** as a restoration path.  Certification requires BOTH
    viability (the mechanism satisfies the three conditions) AND the gain
    contract (gain_sound gives TimelyAdmissibleAfter from viability).
    Without viability, the gain contract is inapplicable.

    The monitor's recommendations are therefore sound but not necessarily
    complete: viable → timely (T1§9), but not timely → viable.              *)
Theorem no_viable_mechanism_blocks_certified_enrichment_recovery :
  forall s,
    HorizonDebt' s ->
    (forall m, ~ viable_enrichment s m) ->
    ~ exists m, (viable_enrichment s m /\ TimelyAdmissibleAfter s m).
Proof.
  intros s _ Hnone [m [Hvie _]].
  exact (Hnone m Hvie).
Qed.

(** Resolution elasticity positivity is necessary for viability.         *)
Theorem viability_requires_positive_elasticity :
  forall s m,
    viable_enrichment s m ->
    resolution_elasticity_positive s m.
Proof.
  intros s m [_ [_ Helast]].
  exact Helast.
Qed.

(** Viability requires authentication.                                    *)
Theorem viability_requires_authentication :
  forall s m,
    viable_enrichment s m ->
    authenticated m.
Proof.
  intros s m [_ [Hauth _]].
  exact Hauth.
Qed.

(** Viability requires the mechanism to arrive before the horizon.        *)
Theorem viability_requires_timely_arrival :
  forall s m,
    viable_enrichment s m ->
    latency m < remaining_horizon s.
Proof.
  intros s m [Hlat _].
  exact Hlat.
Qed.

End ResolutionElasticity.

(* ── §10  Opportunity Cost ────────────────────────────────────────────── *)
(**
   Opportunity cost records the viability rank drop of each mechanism
   between consecutive steps.  A mechanism whose rank fell has the
   following honest reading: it was certified viable under the
   previous-step horizon estimate.  We cannot say it would have worked
   (gain_sound is an axiom, not a real-sensor guarantee); we can say that
   the certified contractual path for that mechanism is now closed.

   OppCost_m(t) = max(0, V_m(t-1) − V_m(t))

   The key theorem (T1§10) states that a positive opportunity cost at
   time t implies the mechanism was previously certified viable — i.e.,
   there existed a step t-1 at which gain_sound's contract applied.     *)

Section OpportunityCost.

(** Abstract viability rank: 0 = NotViable, 1 = Survival, 2 = Reclosure,
    3 = Durable.  Modelled as a natural number.                          *)
Variable viability_rank_at : nat -> nat -> nat.
  (* viability_rank_at t m = rank of mechanism m at time t *)

(** A mechanism was certified viable at step t if its rank was ≥ 1.     *)
Definition certified_viable (t : nat) (m : nat) : Prop :=
  viability_rank_at t m >= 1.

(** Opportunity loss: the rank drop from t-1 to t is positive.          *)
Definition opportunity_loss (t : nat) (m : nat) : Prop :=
  viability_rank_at (t - 1) m > viability_rank_at t m.

(** (T1§10) Positive opportunity loss implies previous certification.

    If the rank strictly fell between t-1 and t, then the rank at t-1
    was at least 1, so the mechanism was certified viable at t-1.
    Coq proves this directly from the arithmetic ordering.               *)
Theorem missed_viable_enrichment_was_certified_repair :
  forall t m,
    opportunity_loss t m ->
    certified_viable (t - 1) m.
Proof.
  intros t m Hloss.
  unfold certified_viable.
  unfold opportunity_loss in Hloss.
  lia.
Qed.

(** (T2§10) Opportunity loss corollary: a mechanism with zero rank at
    t was non-zero at t-1.

    This is the direct contrapositive: if no rank was ever lost (rank at
    t = rank at t-1) then there is no opportunity cost.                  *)
Corollary opportunity_loss_certifies_missed_repair :
  forall t m,
    viability_rank_at t m = 0 ->
    opportunity_loss t m ->
    viability_rank_at (t - 1) m >= 1.
Proof.
  intros t m Hzero Hloss.
  unfold opportunity_loss in Hloss.
  lia.
Qed.

End OpportunityCost.

(* ═══════════════════════════════════════════════════════════════════════
   End of STAK-PSAL formal specification.
   The OCaml toy demonstrator in bin/main.ml exercises these invariants
   dynamically, emitting a JSON Rupture Certificate when §5 is witnessed.
   §8 adds the velocity-aware layer: the monitor escalates immediately
   when remaining_horizon ≤ admissibility_lag (Horizon Debt), bypassing
   the fixed `steps_in_meta > 2` guard that is otherwise required.
   §9 adds the resolution elasticity layer: at the point of Horizon Debt,
   the monitor emits a Horizon Debt Certificate listing which enrichment
   mechanisms can still restore Timely Admissibility.
   §10 adds the opportunity cost layer: the certificate records which
   mechanisms were certified viable under the previous-step horizon
   estimate but have since aged out or degraded in rank.
   ═══════════════════════════════════════════════════════════════════════ *)
