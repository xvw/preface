(** Functors that generate a suite for a [Writer]. *)

module Suite
    (Req_m : Model.COVARIANT_1)
    (M : Preface_specs.MONAD with type 'a t = 'a Req_m.t)
    (Req_t : Model.T0)
    (Tape : Preface_specs.MONOID with type t = Req_t.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0) : Model.SUITE

module Suite_functor
    (Req_m : Model.COVARIANT_1)
    (M : Preface_specs.FUNCTOR with type 'a t = 'a Req_m.t)
    (Req_t : Model.T0)
    (Tape : Preface_specs.MONOID with type t = Req_t.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) : Model.SUITE

module Suite_invariant
    (Req_m : Model.COVARIANT_1)
    (M : Preface_specs.FUNCTOR with type 'a t = 'a Req_m.t)
    (Req_t : Model.T0)
    (Tape : Preface_specs.MONOID with type t = Req_t.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) : Model.SUITE

module Suite_applicative
    (Req_m : Model.COVARIANT_1)
    (M : Preface_specs.APPLICATIVE with type 'a t = 'a Req_m.t)
    (Req_t : Model.T0)
    (Tape : Preface_specs.MONOID with type t = Req_t.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) : Model.SUITE

module Suite_alternative
    (Req_m : Model.COVARIANT_1)
    (M : Preface_specs.ALTERNATIVE with type 'a t = 'a Req_m.t)
    (Req_t : Model.T0)
    (Tape : Preface_specs.MONOID with type t = Req_t.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) : Model.SUITE

module Suite_monad_plus
    (Req_m : Model.COVARIANT_1)
    (M : Preface_specs.MONAD_PLUS with type 'a t = 'a Req_m.t)
    (Req_t : Model.T0)
    (Tape : Preface_specs.MONOID with type t = Req_t.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0) : Model.SUITE
