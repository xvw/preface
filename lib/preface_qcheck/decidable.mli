(** Functor that generate a suite for a [Decidable]. *)

module Suite
    (R : Model.CONTRAVARIANT_1)
    (F : Preface_specs.DECIDABLE with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) : Model.SUITE
