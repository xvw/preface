(** A [Functor] represents a type that can be mapped over. So we can go from
    ['a t] to ['b t] using a function from ['a] to ['b]. Mapping preserve the
    structure of the input.*)

(** {2 Laws}

    To have a predictable behaviour, the instance of [Functor] must obey some
    laws.

    + [map id = id];
    + [map (f % g) = map f % map g]. *)

(** {1 Minimal definition} *)

(** The minimum definition of a [Functor]. It is by using the combinators of
    this module that the other combinators will be derived. *)
module type WITH_MAP = sig
  type 'a t
  (** The type held by the [Functor]. *)

  include Indexed_functor.WITH_MAP with type ('a, _) t := 'a t
  (** @inline *)
end

(** {1 Structure anatomy} *)

module type CORE = WITH_MAP
(** Basis operations.*)

(** Additional operations. *)
module type OPERATION = sig
  type 'a t
  (** The type held by the [Functor]. *)

  include Indexed_functor.OPERATION with type ('a, _) t := 'a t
  (** @inline *)
end

(** Infix operators. *)
module type INFIX = sig
  type 'a t
  (** The type held by the [Functor]. *)

  include Indexed_functor.INFIX with type ('a, _) t := 'a t
  (** @inline *)
end

(** Syntax operators. *)
module type SYNTAX = sig
  type 'a t
  (** The type held by the [Functor]. *)

  include Indexed_functor.SYNTAX with type ('a, _) t := 'a t
  (** @inline *)
end

(** {1 Complete API} *)

(** The complete interface of a [Functor]. *)
module type API = sig
  (** {1 Type} *)

  type 'a t
  (** The type held by the [Functor]. *)

  (** {1 Functions} *)

  include CORE with type 'a t := 'a t
  (** @inline *)

  include OPERATION with type 'a t := 'a t
  (** @inline *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type 'a t = 'a t

  include INFIX with type 'a t := 'a t
  (** @inline *)

  (** {1 Syntax} *)

  module Syntax : SYNTAX with type 'a t = 'a t

  include SYNTAX with type 'a t := 'a t
  (** @inline *)
end

(** {1 Additional references}

    - {{:https://wiki.haskell.org/Functor} Haskell's documentation of a Functor} *)
