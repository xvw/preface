(** Functor that generate a suite for an [Alt]. *)

module Suite
    (R : Model.COVARIANT_1)
    (F : Preface_specs.ALT with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) : Model.SUITE
