(** Building a {!module:Preface_specs.Free_selective} *)

(** {1 Using the minimal definition} *)

(** {2 Over a Functor}

    Build a {!module-type:Preface_specs.FREE_SELECTIVE} over a
    {!module-type:Preface_specs.FUNCTOR}.*)

module Over_functor (F : Preface_specs.Functor.CORE) :
  Preface_specs.FREE_SELECTIVE with type 'a f = 'a F.t

(** {2 Over an Applicative}

    Build a {!module-type:Preface_specs.FREE_SELECTIVE} over a
    {!module-type:Preface_specs.APPLICATIVE}.*)

module Over_applicative (F : Preface_specs.Applicative.CORE) :
  Preface_specs.FREE_SELECTIVE with type 'a f = 'a F.t

(** {2 Over a Selective}

    Build a {!module-type:Preface_specs.FREE_SELECTIVE} over an
    {!module-type:Preface_specs.SELECTIVE} with a [Natural transformation]which
    extracts the underlying value and a [run] function wich is the
    [Natural transformation] from the underlying applicative using the identity.*)

module Over_selective (F : Preface_specs.Selective.CORE) : sig
  include Preface_specs.FREE_SELECTIVE with type 'a f = 'a F.t

  val run : 'a t -> 'a f
end
