(** Functor that generate a suite for a [Divisible]. *)

module Suite
    (R : Model.CONTRAVARIANT_1)
    (F : Preface_specs.DIVISIBLE with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) : Model.SUITE
