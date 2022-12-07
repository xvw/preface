(** Functor that generate a suite for a [Foldable]. *)

module Suite
    (R : Model.COVARIANT_1)
    (F : Preface_specs.FOLDABLE with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (M : Preface_specs.MONOID with type t = A.t) : Model.SUITE
