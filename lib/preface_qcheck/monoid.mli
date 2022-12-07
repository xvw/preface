(** Functor that generate a suite for a [Monoid]. *)

module Suite
    (R : Model.COVARIANT_0)
    (M : Preface_specs.MONOID with type t = R.t) : Model.SUITE
