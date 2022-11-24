(** Functor that generate a suite for a [Semigroup]. *)

module Suite
    (R : Model.COVARIANT_0)
    (S : Preface_specs.SEMIGROUP with type t = R.t) : Model.SUITE
