(** [Indexed Monad_plus] is a kind of {!module:Monoid} on
    {!module:Indexed_monad}. A [Indexed_monad_plus] is formally a
    {!module:Indexed_monad} with neutral and [combine]. So a
    [Indexed_monad_plus] is also a {!module:Indexed_monad}. *)

(** {1 Minimal definition} *)

(** Minimal interfaces of [Indexed Alternative] without {!module:Indexed_monad}. *)
module type WITH_NEUTRAL_AND_COMBINE = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Monad Plus]. *)

  include
    Indexed_alternative.WITH_NEUTRAL_AND_COMBINE
      with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** Minimal definition using [neutral], [combine], [return], [map] and [join]. *)
module type WITH_MAP_AND_JOIN = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Monad Plus]. *)

  include
    Indexed_monad.WITH_RETURN_MAP_AND_JOIN
      with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  include WITH_NEUTRAL_AND_COMBINE with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** Minimal definition using [neutral], [combine], [return],
    [compose_left_to_right]. *)
module type WITH_KLEISLI_COMPOSITION = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Monad Plus]. *)

  include
    Indexed_monad.WITH_RETURN_AND_KLEISLI_COMPOSITION
      with type ('a, 'index) t := ('a, 'index) t

  include WITH_NEUTRAL_AND_COMBINE with type ('a, 'index) t := ('a, 'index) t
end

(** Minimal definition using [neutral], [combine], [return], [bind]. *)
module type WITH_BIND = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Monad Plus]. *)

  include
    Indexed_monad.WITH_RETURN_AND_BIND
      with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  include WITH_NEUTRAL_AND_COMBINE with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Monad Plus]. *)

  include Indexed_monad.CORE with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  include WITH_NEUTRAL_AND_COMBINE with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** Additional operations. *)
module type OPERATION = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Monad Plus]. *)

  include Indexed_monad.OPERATION with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  include
    Indexed_alternative.ALTERNATIVE_OPERATION
      with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  val filter : ('a -> bool) -> ('a, 'index) t -> ('a, 'index) t
  (** Filtering over [Monad_plus]. *)
end

(** Infix operators. *)
module type INFIX = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Monad Plus]. *)

  include Indexed_monad.INFIX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  val ( <|> ) : ('a, 'index) t -> ('a, 'index) t -> ('a, 'index) t
  (** Infix version of {!val:CORE.combine}. *)
end

(** Syntax extensions *)
module type SYNTAX = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Monad Plus]. *)

  include Indexed_monad.SYNTAX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** {1 Complete API} *)

(** The complete interface of an [Indexed_monad_plus]. *)
module type API = sig
  (** {1 Type} *)

  type ('a, 'index) t
  (** The type held by the [Indexed Monad Plus]. *)

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
