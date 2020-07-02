Require Import Relations.
Require Import FinFun.
Require Import Relation_Operators.

Require Import MathClasses.interfaces.abstract_algebra.

Section Definitions.
  Context {A : Type} (R1 R2 : relation A).

  Definition meet : relation A := fun x y => R1 x y /\ R2 x y.
  Lemma meet_equiv : equivalence A R1 -> equivalence A R2 -> equivalence A meet.
    intros [] [].
    unfold reflexive, symmetric, transitive in *.
    unfold meet.
    repeat constructor; intuition eauto.
  Qed.

  Definition join : relation A := clos_refl_trans A (fun x y => R1 x y \/ R2 x y).
  Lemma join_equiv : equivalence A R1 -> equivalence A R2 -> equivalence A join.
    intros [] [].
    unfold join.
    constructor;
      unfold reflexive, symmetric, transitive in *;
      intuition eauto.
    - apply rt_step; eauto.
    - eapply rt_trans; eauto.
    - induction H; intuition.
        eapply rt_trans; eassumption.
  Qed.
End Definitions.

Section Lattice.
  Context {A : Type}.

  Definition rel_iff (R S : relation A) : Prop := forall x y, R x y <-> S x y.
  Instance rel_iff_Equiv : Equiv (relation A) := rel_iff.
  Instance rel_iff_Equivalence : Equivalence rel_iff.
    unfold rel_iff.
    do 2 constructor; intuition eauto.
    - apply H; assumption.
    - apply H; assumption.
    - apply H0, H; assumption.
    - apply H, H0; assumption.
  Qed.
  Instance rel_iff_Setoid : Setoid (relation A) := rel_iff_Equivalence.

  Instance equiv_meet_Meet : Meet (relation A) := meet.
  Instance equiv_join_Join : Join (relation A) := join.
  Instance equiv_meet_Associative : Associative (@meet A). constructor; unfold meet in *; tauto. Qed.
  Instance equiv_join_Associative : Associative (@join A).
  Proof using.
    constructor.
    rename x into R, y into S, z into T.
    - induction 1 as []; unfold join in *.
      + destruct H as [ | []]; intuition.
        eapply rt_trans; [clear H0 | clear H].
        * induction H.
          -- destruct H; repeat constructor; eauto; fail.
          -- apply rt_refl.
          -- eapply rt_trans; eassumption.
        * induction H0.
          -- destruct H; repeat constructor; eauto; fail.
          -- apply rt_refl.
          -- eapply rt_trans; eassumption.
      + apply rt_refl.
      + eapply rt_trans; eassumption.
    - induction 1 as []; unfold join in *.
      + destruct H as [[] | ]; intuition.
        eapply rt_trans; [clear H0 | clear H].
        * induction H.
          -- destruct H; repeat constructor; eauto; fail.
          -- apply rt_refl.
          -- eapply rt_trans; eassumption.
        * induction H0.
          -- destruct H; repeat constructor; eauto; fail.
          -- apply rt_refl.
          -- eapply rt_trans; eassumption.
      + apply rt_refl.
      + eapply rt_trans; eassumption.
  Qed.

  Instance equiv_meet_Proper : Proper (equiv ==> equiv ==> equiv) equiv_meet_Meet. firstorder. Qed.

  Instance equiv_join_Proper : Proper (equiv ==> equiv ==> equiv) equiv_join_Join.
  Proof using.
    unfold Proper, respectful.
    intros R S H R' S' H'.
    unfold equiv_join_Join, join, equiv, rel_iff_Equiv, rel_iff in *.
    intros x y.
    split; destruct 1 as [? [] | | ].
    - apply rt_step; left; apply H; assumption.
    - apply rt_step; right; apply H'; assumption.
    - apply rt_refl.
    - eapply rt_trans; [clear H0_0 | clear H0_].
      + induction H0_.
        * rewrite H, H' in H0.
          apply rt_step.
          eassumption.
        * apply rt_refl.
        * eapply rt_trans; eassumption.
      + induction H0_0.
        * rewrite H, H' in H0.
          apply rt_step.
          eassumption.
        * apply rt_refl.
        * eapply rt_trans; eassumption.
    - apply rt_step; left; apply H; assumption.
    - apply rt_step; right; apply H'; assumption.
    - apply rt_refl.
    - eapply rt_trans; [clear H0_0 | clear H0_].
      + induction H0_.
        * rewrite <-H, <-H' in H0.
          apply rt_step.
          eassumption.
        * apply rt_refl.
        * eapply rt_trans; eassumption.
      + induction H0_0.
        * rewrite <-H, <-H' in H0.
          apply rt_step.
          eassumption.
        * apply rt_refl.
        * eapply rt_trans; eassumption.
  Qed.

  Instance equiv_join_SemiGroup : SemiGroup (relation A) :=
    { sg_setoid := rel_iff_Setoid;
      sg_ass := equiv_join_Associative;
      sg_op_proper := equiv_join_Proper
    }.

  Instance equiv_join_Commutative : Commutative equiv_join_Join.
  Proof using.
    unfold Commutative.
    intros R S x y.
    unfold equiv_join_Join, join, equiv, rel_iff_Equiv, rel_iff.
    assert (forall x y, (R x y \/ S x y) <->
                   (S x y \/ R x y))
      as Hiff
      by intuition.

  Instance equiv_Lattice : Lattice (relation A).
  Proof.
    constructor.
    - constructor.
      + constructor.
        * typeclasses eauto.
