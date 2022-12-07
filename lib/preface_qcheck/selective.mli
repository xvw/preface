(** Functors that generate a suite for a [Selective]. *)

module Suite
    (R : Model.COVARIANT_1)
    (F : Preface_specs.SELECTIVE with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) : Model.SUITE

module Suite_rigid
    (R : Model.COVARIANT_1)
    (F : Preface_specs.SELECTIVE with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) : Model.SUITE
