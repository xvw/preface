(** A [Monad] - TODO *)

module type CORE_VIA_BIND = sig
  type 'a t
  (** The type holded by the [Monad]. *)

  val return : 'a -> 'a t
  (** Create a new ['a t]. *)

  val bind : ('a -> 'b t) -> 'a t -> 'b t
end

module type CORE_VIA_MAP_AND_JOIN = sig
  type 'a t
  (** The type holded by the [Monad]. *)

  val return : 'a -> 'a t
  (** Create a new ['a t]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** Mapping over from ['a] to ['b] over ['a t] to ['b t]. *)

  val join : 'a t t -> 'a t
end

module type CORE_VIA_KLEISLI_COMPOSITION = sig
  type 'a t
  (** The type holded by the [Monad]. *)

  val return : 'a -> 'a t
  (** Create a new ['a t]. *)

  val compose_left_to_right : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t
end

module type CORE = sig
  include CORE_VIA_BIND

  include CORE_VIA_MAP_AND_JOIN with type 'a t := 'a t

  include CORE_VIA_KLEISLI_COMPOSITION with type 'a t := 'a t
end

module type OPERATION = sig
  type 'a t
  (** The type holded by the [Monad]. *)
end

module type SYNTAX = sig
  type 'a t
  (** The type holded by the [Monad]. *)
end

module type INFIX = sig
  type 'a t
  (** The type holded by the [Monad]. *)
end

(** {1 API} *)

(** The complete interface of a [Monad]. *)
module type API = sig
  include CORE

  include OPERATION with type 'a t := 'a t

  module Syntax : SYNTAX with type 'a t := 'a t

  include module type of Syntax

  module Infix : INFIX with type 'a t := 'a t

  include module type of Infix
end
