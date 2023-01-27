(** An [Indexed Functor] represents a type that can be mapped over (with an
    index, so an additional type parameter that can be useful for tracking
    information using row polymorphism). So we can go from [('a, 'index) t] to
    [('b, 'index) t] using a function from ['a] to ['b]. Mapping preserve the
    structure of the input. *)

(** {1 Minimal definition} *)

(** {1 Minimal definition} *)

(** The minimum definition of an [Indexed Functor]. It is by using the
    combinators of this module that the other combinators will be derived. *)
module type WITH_MAP = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Functor]. *)

  val map : ('a -> 'b) -> ('a, 'index) t -> ('b, 'index) t
  (** Mapping over from ['a] to ['b]. *)
end

(** {1 Structure anatomy} *)

module type CORE = WITH_MAP
(** Basis operations.*)

(** Additional operations. *)
module type OPERATION = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Functor]. *)

  val replace : 'a -> ('b, 'index) t -> ('a, 'index) t
  (** Create a new [t], replacing all values of the given functor by given a
      value of ['a]. *)

  val void : ('a, 'index) t -> (unit, 'index) t
  (** Create a new [t], replacing all values in the given functor by [unit]. *)
end

(** Infix operators. *)
module type INFIX = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Functor]. *)

  val ( <$> ) : ('a -> 'b) -> ('a, 'index) t -> ('b, 'index) t

  (** Infix version of {!val:CORE.map}. *)

  val ( <&> ) : ('a, 'index) t -> ('a -> 'b) -> ('b, 'index) t

  (** Flipped and infix version of {!val:CORE.map}. *)

  val ( <$ ) : 'a -> ('b, 'index) t -> ('a, 'index) t
  (** Infix version of {!val:OPERATION.replace}. *)

  val ( $> ) : ('a, 'index) t -> 'b -> ('b, 'index) t

  (** Flipped and infix version of {!val:OPERATION.replace}. *)
end

(** Syntax operators. *)
module type SYNTAX = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Functor]. *)

  val ( let+ ) : ('a, 'index) t -> ('a -> 'b) -> ('b, 'index) t
  (** [let] operator for mapping. *)
end

(** {1 Complete API} *)

(** The complete interface of a [Functor]. *)
module type API = sig
  (** {1 Type} *)

  type ('a, 'index) t
  (** The type held by the [Indexed Functor]. *)

  (** {1 Functions} *)

  include CORE with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  include OPERATION with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type ('a, 'index) t = ('a, 'index) t

  include INFIX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  (** {1 Syntax operators} *)

  module Syntax : SYNTAX with type ('a, 'index) t = ('a, 'index) t

  include SYNTAX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end
