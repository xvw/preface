(** Functors that generate a suite for a [Monad_plus]. *)

module Suite
    (R : Model.COVARIANT_1)
    (F : Preface_specs.MONAD_PLUS with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0) : Model.SUITE

module Suite_monoidal
    (R : Model.COVARIANT_1)
    (F : Preface_specs.MONAD_PLUS with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0) : Model.SUITE

module Suite_left_absorption
    (R : Model.COVARIANT_1)
    (F : Preface_specs.MONAD_PLUS with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0) : Model.SUITE

module Suite_left_distributivity
    (R : Model.COVARIANT_1)
    (F : Preface_specs.MONAD_PLUS with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0) : Model.SUITE

module Suite_left_catch
    (R : Model.COVARIANT_1)
    (F : Preface_specs.MONAD_PLUS with type 'a t = 'a R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0) : Model.SUITE
