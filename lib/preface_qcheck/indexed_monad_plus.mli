(** Functors that generate a suite for an [Indexed_monad_plus]. *)

module Suite
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_MONAD_PLUS
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0)
    (Index : Model.T0) : Model.SUITE

module Suite_monoidal
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_MONAD_PLUS
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0)
    (Index : Model.T0) : Model.SUITE

module Suite_left_absorption
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_MONAD_PLUS
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0)
    (Index : Model.T0) : Model.SUITE

module Suite_left_distributivity
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_MONAD_PLUS
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0)
    (Index : Model.T0) : Model.SUITE

module Suite_left_catch
    (R : Model.COVARIANT_2)
    (F : Preface_specs.INDEXED_MONAD_PLUS
           with type ('a, 'index) t = ('a, 'index) R.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0)
    (Index : Model.T0) : Model.SUITE
