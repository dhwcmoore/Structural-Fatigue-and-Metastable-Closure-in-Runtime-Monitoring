Require Import Reals Lra Lia.
Open Scope R_scope.
Variable q : R.
Goal forall k : nat, q ^ (S k) = q * q ^ k.
Proof. intro k. simpl. reflexivity. Qed.
