(** Functor that generate a suite for an [Arrow Alt]. *)

module Suite
    (R : Model.PROFUNCTORIAL)
    (P : Preface_specs.ARROW_ALT with type ('a, 'b) t = ('a, 'b) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0) : Model.SUITE
