(** A [Functor] represents a type that can be mapped over. So we can go from
    ['a t] to ['b t] using a function from ['a] to ['b]. Mapping preserve the
    structure of the input.*)

(** {2 Laws}

    To have a predictable behaviour, the instance of [Functor] must obey some
    laws.

    + [map id = id];
    + [map (f % g) = map f % map g]. *)

(** {1 Structure anatomy} *)

(** The minimum definition of a [Functor]. It is by using the combinators of
    this module that the other combinators will be derived. *)
module type CORE = sig
  type 'a t
  (** The type held by the [Functor]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** Mapping over from ['a] to ['b] over ['a t] to ['b t]. *)
end

(** Additional operations. *)
module type OPERATION = sig
  type 'a t
  (** The type held by the [Functor]. *)

  val replace : 'a -> 'b t -> 'a t
  (** Create a new ['a t], replacing all values in the ['b t] by given a value
      of ['a]. *)

  val void : 'a t -> unit t
  (** Create a new [unit t], replacing all values in the ['a t] by [unit]. *)
end

(** Infix operators. *)
module type INFIX = sig
  type 'a t
  (** The type held by the [Functor]. *)

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
  (** Infix version of {!val:CORE.map}. *)

  val ( <&> ) : 'a t -> ('a -> 'b) -> 'b t
  (** Flipped and infix version of {!val:CORE.map}. *)

  val ( <$ ) : 'a -> 'b t -> 'a t
  (** Infix version of {!val:OPERATION.replace}. *)

  val ( $> ) : 'a t -> 'b -> 'b t
  (** Flipped and infix version of {!val:OPERATION.replace}. *)
end

(** {1 Complete API} *)

(** The complete interface of a [Functor]. *)
module type API = sig
  (** {1 Core functions}

      Set of fundamental functions in the description of a [Functor]. *)

  include CORE
  (** @closed *)

  (** {1 Additional functions}

      Additional functions, derived from fundamental functions. *)

  include OPERATION with type 'a t := 'a t
  (** @closed *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type 'a t := 'a t

  (** {2 Infix operators inclusion} *)

  include module type of Infix
  (** @closed *)
end

(** {1 Additional references}

    - {{:https://wiki.haskell.org/Functor} Haskell's documentation of a Functor} *)
