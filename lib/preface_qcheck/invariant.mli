(** Functors that generate a suite for an [Invariant]. *)

module Suite
    (R : Model.COVARIANT_1)
    (I : Preface_specs.INVARIANT with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) : Model.SUITE

module Suite_contravariant
    (R : Model.CONTRAVARIANT_1)
    (I : Preface_specs.INVARIANT with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) : Model.SUITE
