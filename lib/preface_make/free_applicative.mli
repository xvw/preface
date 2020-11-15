(** Modules for building [Free_applicative] modules. *)

(** {1 Building a free applicative over a functor}

    Given a functor, it produce an applicative module with a natural
    transformation in order to extract the underlying value. *)

module Over_functor (F : Preface_specs.Functor.CORE) :
  Preface_specs.FREE_APPLICATIVE with type 'a f = 'a F.t

(** {1 Building a free applicative over an applicative}

    Given a functor, it produce an applicative module with a natural
    transformation in order to extract the underlying value and a [run] function
    wich is the natural transformation from the underlying applicative using the
    identity. *)

module Over_applicative (A : Preface_specs.Applicative.CORE) : sig
  include Preface_specs.FREE_APPLICATIVE with type 'a f = 'a A.t

  val run : 'a t -> 'a f
end
