(** A [Functor] for ['a t] is mappable from ['a] to ['b].
    So for all ['a t], we can go to ['b t] using the [map] function.

    To have a predictable behaviour, the instance of [Functor] must
    obey some laws.

    {1 Laws of [Functor]}
    - [fmap id] must be equivalent to [id];
    - [fmap (f <% g)] must be equivalent to [fmap f <% fmap g].
*)

(** {1 Requirement} *)

(** Standard requirement. *)
module type REQUIREMENT = sig
  type 'a t
  (** The type holded by the [Functor]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** Mapping over from ['a] to ['b] over ['a t] to ['b t]. *)
end

(** Requirement with replace *)
module type FULL_REQUIREMENT = sig
  include REQUIREMENT

  val replace : 'a -> 'b t -> 'a t
  (** Create a new ['a t], replacing all values in the ['b t]
      by given a value of ['a].
  *)
end

(** {1 API} *)

(** The complete interface of a [Functor]. *)
module type API = sig
  include FULL_REQUIREMENT

  val void : 'a t -> unit t
  (** Create a new [unit t], replacing all values in the ['a t] by [unit]. *)

  module Infix : sig
    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
    (** Infix version of {!val:map}. *)

    val ( <&> ) : 'a t -> ('a -> 'b) -> 'b t
    (** Flipped and infix version of {!val:map}. *)

    val ( <$ ) : 'a -> 'b t -> 'a t
    (** Infix version of {!val:replace}. *)

    val ( $> ) : 'a t -> 'b -> 'b t
    (** Flipped and infix version of {!val:replace}. *)
  end

  include module type of Infix
end
