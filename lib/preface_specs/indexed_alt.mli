(** [Indexed Alt] is an {!module:Indexed_functor} which is a kind of
    {!module:Semigroup} over a parametrized type. In other word, [Indexed_alt]
    is a {!module:Indexed_functor} with a [combine] operation. *)

(** {1 Minimal definition} *)

(** Combine operation. This signature is mainly used to enrich an
    {!module:Indexed_functor} with [combine].*)
module type WITH_COMBINE = sig
  type ('a, 'index) t
  (** A type [('a, 'index) t] held by the [Alt]. *)

  val combine : ('a, 'index) t -> ('a, 'index) t -> ('a, 'index) t
  (** Combine two values of [t] into one. *)
end

(** The minimum definition of an [Indexed Alt]. It is by using the combinators
    of this module that the other combinators will be derived. *)
module type WITH_COMBINE_AND_MAP = sig
  include WITH_COMBINE
  (** @inline *)

  include Indexed_functor.WITH_MAP with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** {1 Structure anatomy} *)

module type CORE = WITH_COMBINE_AND_MAP
(** Basis operations.*)

(** Additional operations. *)
module type OPERATION = sig
  type ('a, 'index) t
  (** A type [('a, 'index) t] held by the [Indexed Alt]. *)

  val times_nel : int -> ('a, 'index) t -> ('a, 'index) t option
  (** [times_nel n x] apply [combine] on [x] [n] times. If [n] is lower than [1]
      the function will returns [None]. *)

  val reduce_nel : ('a, 'index) t Preface_core.Nonempty_list.t -> ('a, 'index) t
  (** Reduce a [Nonempty_list.t] using [combine]. *)

  include Indexed_functor.OPERATION with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** Infix operators. *)
module type INFIX = sig
  type ('a, 'index) t
  (** A type [('a, 'index) t] which is an [Alt]. *)

  val ( <|> ) : ('a, 'index) t -> ('a, 'index) t -> ('a, 'index) t
  (** Infix version of {!val:CORE.combine} *)

  include Indexed_functor.INFIX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** Syntax operators. *)
module type SYNTAX = sig
  type ('a, 'index) t
  (** A type [('a, 'index) t] which is an [Indexed_alt]. *)

  include Indexed_functor.SYNTAX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** {1 Complete API} *)

(** The complete interface of an [Indexed_alt]. *)
module type API = sig
  (** {1 Type} *)

  type ('a, 'index) t
  (** The type held by the [Indexed Alt]. *)

  (** {1 Functions} *)

  include CORE with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  include OPERATION with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type ('a, 'index) t := ('a, 'index) t

  include INFIX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  (** {1 Syntax operators} *)

  module Syntax : SYNTAX with type ('a, 'index) t := ('a, 'index) t

  include SYNTAX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end
