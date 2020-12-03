(** A [Free_selective] allows you to build a rigid selective from a given
    functor. Such selective is equiped with and additional function for
    promoting values from the underlying functor into the selective and a
    natural transformation in order to unwrapping the value from the free
    selective. *)

(** {1 Structure anatomy} *)

(** Standard requirement. *)
module type CORE = sig
  type 'a f
  (** The type held by the [Functor]. *)

  (** The type held by the [Free_selective]. *)

  type _ t =
    | Pure : 'a -> 'a t
    | Select : ('a, 'b) Either.t t * ('a -> 'b) f -> 'b t

  val promote : 'a f -> 'a t
  (** Promote a value from the functor into the free selective. *)

  (** The natural transformation from a free selective to an other selective. *)
  module Transformation (Selective : Selective.CORE) : sig
    type natural_transformation = { transform : 'a. 'a f -> 'a Selective.t }

    val run : natural_transformation -> 'a t -> 'a Selective.t
    (** Run the natural transformation over the free selective. *)
  end
end

(** {1 API} *)

(** The complete interface of a [Free_applicative]. *)
module type API = sig
  include CORE

  include Selective.API with type 'a t := 'a t
end

(** {1 Bibliography}

    - {{:http://hackage.haskell.org/package/selective} Haskell's documentation
      of a Selective Application Functor}
    - {{:https://www.staff.ncl.ac.uk/andrey.mokhov/selective-functors.pdf}
      Selective Applicative Functors} *)
