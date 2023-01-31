(** Functor that generate a suite for an [Indexed Foldable]. *)

module Suite
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_FOLDABLE
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (M : Preface_specs.MONOID with type t = A.t)
    (Index : Model.T0) : Model.SUITE
