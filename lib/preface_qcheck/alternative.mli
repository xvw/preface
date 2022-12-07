(** Functors that generate a suite for an [Alternative]. *)

module Suite
    (R : Model.COVARIANT_1)
    (F : Preface_specs.ALTERNATIVE with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) : Model.SUITE

module Suite_monoidal
    (R : Model.COVARIANT_1)
    (F : Preface_specs.ALTERNATIVE with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) : Model.SUITE

module Suite_right_distributivity
    (R : Model.COVARIANT_1)
    (F : Preface_specs.ALTERNATIVE with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) : Model.SUITE

module Suite_right_absorbtion
    (R : Model.COVARIANT_1)
    (F : Preface_specs.ALTERNATIVE with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) : Model.SUITE
