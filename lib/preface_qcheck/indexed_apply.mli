(** Functor that generate a suite for an [Indexed Apply]. *)

module Suite
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_APPLY with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (Index : Model.T0) : Model.SUITE
