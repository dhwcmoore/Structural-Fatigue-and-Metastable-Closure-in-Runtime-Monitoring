Require Import Reals.
Require Import Lra.
Require Import Lia.

Open Scope R_scope.

Section ContractionCore.
  Variable q : R.
  Hypothesis q_pos : 0 < q.
  Hypothesis q_lt_1 : q < 1.

  Variable d_seq : nat -> R.
  Hypothesis d_nonneg : forall t, 0 <= d_seq t.
  Hypothesis contraction_step : forall t, d_seq (S t) <= q * d_seq t.

  (** The fundamental geometric collapse lemma *)
  Theorem contraction_multi_step :
    forall t k, d_seq (t + k)%nat <= (q ^ k) * d_seq t.
  Proof.
    intros t k.
    induction k as [| k IH].
    - rewrite Nat.add_0_r. simpl. lra.
    - replace (t + S k)%nat with (S (t + k))%nat by lia.
      change (q ^ S k) with (q * q ^ k).
      specialize (contraction_step (t + k)%nat) as Hstep.
      eapply Rle_trans; [exact Hstep |].
      rewrite Rmult_assoc.
      apply Rmult_le_compat_l; [lra | exact IH].
  Qed.
End ContractionCore.
