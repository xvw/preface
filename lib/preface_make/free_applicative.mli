(** Building a {!module:Preface_specs.Free_applicative} *)

(** {1 Using the minimal definition} *)

(** {2 Over a Functor}

    Build a {!module-type:Preface_specs.FREE_APPLICATIVE} over a
    {!module-type:Preface_specs.FUNCTOR}.*)

module Over_functor (F : Preface_specs.Functor.CORE) :
  Preface_specs.FREE_APPLICATIVE with type 'a f = 'a F.t

(** {2 Over an Applicative}

    Build a {!module-type:Preface_specs.FREE_APPLICATIVE} over an
    {!module-type:Preface_specs.APPLICATIVE} with a
    [Natural transformation]which extracts the underlying value and a [run]
    function wich is the [Natural transformation] from the underlying
    applicative using the identity.*)

module Over_applicative (A : Preface_specs.Applicative.CORE) : sig
  include Preface_specs.FREE_APPLICATIVE with type 'a f = 'a A.t

  val run : 'a t -> 'a f
end
