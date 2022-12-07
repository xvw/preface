(** Functor that generate a suite for a [Semigroupoid]. *)

module Suite
    (R : Model.PROFUNCTORIAL)
    (P : Preface_specs.SEMIGROUPOID with type ('a, 'b) t = ('a, 'b) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (C : Model.T0) : Model.SUITE
