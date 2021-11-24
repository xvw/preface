(** Building a {!module:Preface_specs.Freer_selective} *)

(** {1 Using the minimal definition}

    Build a {!module-type:Preface_specs.FREER_SELECTIVE} over an arbitrary type
    with one type parameter ( {!module-type:Preface_specs.Types.T1}).*)

module Over (T : Preface_specs.Types.T1) :
  Preface_specs.FREER_SELECTIVE with type 'a f = 'a T.t

(** {2 Over a Selective}

    Build a {!module-type:Preface_specs.FREER_SELECTIVE} over an
    {!module-type:Preface_specs.SELECTIVE} with a [Natural transformation] which
    extracts the underlying value and a [run] function wich is the
    [Natural transformation] from the underlying applicative using the identity.*)

module Over_selective (F : Preface_specs.SELECTIVE) : sig
  include Preface_specs.FREER_SELECTIVE with type 'a f = 'a F.t

  val run : 'a t -> 'a f
end
