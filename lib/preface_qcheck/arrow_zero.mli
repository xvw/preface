(** Functor that generate a suite for an [Arrow Zero]. *)

module Suite
    (R : Model.PROFUNCTORIAL)
    (P : Preface_specs.ARROW_ZERO with type ('a, 'b) t = ('a, 'b) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0) : Model.SUITE
