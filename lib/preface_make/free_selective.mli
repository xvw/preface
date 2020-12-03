(** Modules for building [Free_selective] modules. *)

(** {2 Building a free rigid selective over a functor}

    Given a functor, it produce a selective module with a natural transformation
    in order to extract the underlying value. *)

module Over_functor (F : Preface_specs.Functor.CORE) :
  Preface_specs.FREE_SELECTIVE with type 'a f = 'a F.t

(** {2 Building a free applicative over an applicative}

    Given an applicative, it produce a selective module with a natural
    transformation in order to extract the underlying value. *)

module Over_applicative (F : Preface_specs.Applicative.CORE) :
  Preface_specs.FREE_SELECTIVE with type 'a f = 'a F.t

(** {2 Building a free selective over a selective}

    Given a selective, it produce a selective module with a natural
    transformation in order to extract the underlying value and a [run] function
    wich is the natural transformation from the underlying selective using the
    identity. *)

module Over_selective (F : Preface_specs.Selective.CORE) : sig
  include Preface_specs.FREE_SELECTIVE with type 'a f = 'a F.t

  val run : 'a t -> 'a f
end
