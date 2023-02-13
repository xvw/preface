(** An [Indexed Monad] allow to sequences operations that are dependent from one
    to another, in contrast to {!module:Indexed_applicative}, which executes a
    series of independent actions.*)

(** {1 Minimal definition} *)

(** Minimal interface using [map] and [product]. *)
module type WITH_RETURN = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Monad]. *)

  val return : 'a -> ('a, 'index) t
  (** Lift a value into a [t]. *)
end

(** Minimal definition using [return] and [bind]. *)
module type WITH_RETURN_AND_BIND = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Monad]. *)

  include WITH_RETURN with type ('a, 'index) t := ('a, 'index) t
  include Indexed_bind.WITH_BIND with type ('a, 'index) t := ('a, 'index) t
end

(** Minimal definition using [return], [map] and [join]. *)
module type WITH_RETURN_MAP_AND_JOIN = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Monad]. *)

  include WITH_RETURN with type ('a, 'index) t := ('a, 'index) t

  include
    Indexed_bind.WITH_MAP_AND_JOIN with type ('a, 'index) t := ('a, 'index) t
end

(** Minimal definition using [return] and [compose_left_to_right]. *)
module type WITH_RETURN_AND_KLEISLI_COMPOSITION = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Monad]. *)

  include WITH_RETURN with type ('a, 'index) t := ('a, 'index) t

  include
    Indexed_bind.WITH_KLEISLI_COMPOSITION
      with type ('a, 'index) t := ('a, 'index) t
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  include WITH_RETURN_AND_BIND
  (** @inline *)

  include WITH_RETURN_MAP_AND_JOIN with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  include
    WITH_RETURN_AND_KLEISLI_COMPOSITION
      with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** Additional operations. *)
module type OPERATION = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Monad]. *)

  include Indexed_bind.OPERATION with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** Syntax extensions. *)
module type SYNTAX = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Monad]. *)

  include Indexed_bind.SYNTAX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** Infix operators. *)
module type INFIX = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Monad]. *)

  include Indexed_bind.INFIX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** {1 Complete API} *)

(** The complete interface of a [Indexed Monad]. *)
module type API = sig
  (** {1 Type} *)

  type ('a, 'index) t
  (** The type held by the [Indexed Monad]. *)

  (** {1 Functions} *)

  include CORE with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  include OPERATION with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type ('a, 'index) t := ('a, 'index) t

  include INFIX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  (** {1 Syntax} *)

  module Syntax : SYNTAX with type ('a, 'index) t := ('a, 'index) t

  include SYNTAX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end
