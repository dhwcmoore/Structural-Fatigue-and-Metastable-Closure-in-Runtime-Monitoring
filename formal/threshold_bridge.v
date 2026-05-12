Require Import Reals.
Require Import Lra.
Require Import Lia.
Require Import contraction_core.

Open Scope R_scope.

Section ThresholdBridge.
  (* 1. Parameters restated to ensure local visibility *)
  Variable q : R.
  Hypothesis q_pos : 0 < q.
  Hypothesis q_lt_1 : q < 1.

  Variable d_seq : nat -> R.
  Hypothesis d_nonneg : forall t, 0 <= d_seq t.
  Hypothesis contraction_step : forall t, d_seq (S t) <= q * d_seq t.

  (* Analytic Convergence Hypothesis *)
  Hypothesis geometric_power_eventually_small :
    forall r, 0 < r -> exists k : nat, q ^ k <= r.

  (* Local instantiation of the core engine *)
  Definition Hmulti := contraction_multi_step q q_pos d_seq contraction_step.

  (* The threshold crossing theorem remains as established *)
  Theorem threshold_crossing :
    forall t eps eps',
      0 < eps -> 0 < eps' -> eps' < eps ->
      d_seq t <= eps ->
      exists k : nat, d_seq (t + k)%nat <= eps'.
  Proof.
    intros t eps eps' Heps Heps' Hrange Hstart.
    assert (Hr_pos : 0 < eps' / eps) by (apply Rdiv_lt_0_compat; lra).
    destruct (geometric_power_eventually_small (eps' / eps) Hr_pos) as [k Hk].
    exists k.
    eapply Rle_trans.
    - apply Hmulti.
    - replace eps' with ((eps' / eps) * eps) by (field; lra).
      apply Rmult_le_compat.
      + apply pow_le; lra.
      + exact (d_nonneg t).
      + exact Hk.
      + exact Hstart.
  Qed.

End ThresholdBridge.
