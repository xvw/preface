(** Building a {!module:Preface_specs.Freer_monad} *)

(** {1 Using the minimal definition}

    Build a {!module-type:Preface_specs.FREER_MONAD} over an arbitrary type with
    one type parameter ( {!module-type:Preface_specs.Types.T1}).*)

module Over (T : Preface_specs.Types.T1) :
  Preface_specs.FREER_MONAD with type 'a f = 'a T.t
