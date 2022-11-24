(** Functor that generate a suite for a [Closed Profunctor]. *)

module Suite
    (R : Model.PROFUNCTORIAL)
    (P : Preface_specs.CLOSED with type ('a, 'b) t = ('a, 'b) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0)
    (E : Model.T0)
    (F : Model.T0) : Model.SUITE
