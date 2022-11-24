(** Functor that generate a suite for a [Contravariant Functor]. *)

module Suite
    (R : Model.CONTRAVARIANT_1)
    (F : Preface_specs.CONTRAVARIANT with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) : Model.SUITE
