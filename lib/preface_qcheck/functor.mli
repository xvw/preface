(** Functor that generate a suite for a [Functor]. *)

module Suite
    (R : Model.COVARIANT_1)
    (F : Preface_specs.FUNCTOR with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) : Model.SUITE
