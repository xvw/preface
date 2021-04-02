(** A [Free_applicative] allows you to build an applicative from a given
    functor. Such applicative is equiped with and additional function for
    promoting values from the underlying functor into the free applicative and a
    natural transformation in order to unwrapping the value from the free
    applicative. *)

(** {1 Structure anatomy} *)

(** Standard requirement. *)
module type CORE = sig
  type 'a f
  (** The type held by the [Functor]. *)

  (** The type held by the [Free_applicative]. *)
  type _ t =
    | Pure : 'a -> 'a t
    | Apply : ('a -> 'b) t * 'a f -> 'b t

  val promote : 'a f -> 'a t
  (** Promote a value from the functor into the free applicative. *)

  (** The natural transformation from a free applicative to an other
      applicative. *)
  module To_applicative (Applicative : Applicative.CORE) : sig
    type natural_transformation = { transform : 'a. 'a f -> 'a Applicative.t }

    val run : natural_transformation -> 'a t -> 'a Applicative.t
    (** Run the natural transformation over the free applicative. *)
  end

  (** The natural transformation from a free applicative to a monoid. *)
  module To_monoid (Monoid : Monoid.CORE) : sig
    type natural_transformation = { transform : 'a. 'a f -> Monoid.t }

    val run : natural_transformation -> 'a t -> Monoid.t
    (** Run the natural transformation over the free applicative. *)
  end
end

(** {1 API} *)

(** The complete interface of a [Free_applicative]. *)
module type API = sig
  include CORE

  include Applicative.API with type 'a t := 'a t
end

(** {1 Bibliography}

    - {{:https://arxiv.org/pdf/1403.0749.pdf} Free Applicative Functors} *)
