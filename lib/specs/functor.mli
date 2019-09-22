(** A [Functor] for ['a t] is mappable from ['a] to ['b].
    So for all ['a t], we can go to ['b t] using the [map] function.

    To have a predictable behaviour, the instance of [Functor] must
    obey some laws.

    {1 Laws of [Functor]}
    - [fmap id] must be equivalent to [id];
    - [fmap (f <% g)] must be equivalent to [fmap f <% fmap g].
*)

(** {1 Structure anatomy} *)

(** Standard requirement. *)
module type CORE = sig
  type 'a t
  (** The type holded by the [Functor]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** Mapping over from ['a] to ['b] over ['a t] to ['b t]. *)
end

(** Operations *)
module type OPERATION = sig
  type 'a t
  (** The type holded by the [Functor]. *)

  val replace : 'a -> 'b t -> 'a t
  (** Create a new ['a t], replacing all values in the ['b t]
      by given a value of ['a].
  *)

  val void : 'a t -> unit t
  (** Create a new [unit t], replacing all values in the ['a t] by [unit]. *)
end

(** Infix notation *)
module type INFIX = sig
  type 'a t
  (** The type holded by the [Functor]. *)

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
  (** Infix version of {!val:map}. *)

  val ( <&> ) : 'a t -> ('a -> 'b) -> 'b t
  (** Flipped and infix version of {!val:map}. *)

  val ( <$ ) : 'a -> 'b t -> 'a t
  (** Infix version of {!val:replace}. *)

  val ( $> ) : 'a t -> 'b -> 'b t
  (** Flipped and infix version of {!val:replace}. *)
end

(** {1 API} *)

(** The complete interface of a [Functor]. *)
module type API = sig
  include CORE

  include OPERATION with type 'a t := 'a t

  module Infix : INFIX with type 'a t := 'a t

  include module type of Infix
end
