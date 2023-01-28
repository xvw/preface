(** An [Indexed Selective] (applicative functor) allows to declare effects
    statically and select which execute dynamically. It is an algebraic
    structure between {!module:Indexed_applicative} and {!module:Indexed_monad}.
    An [Indexed_selective] is also an {!module:Indexed_applicative}. *)

(** {1 Minimal definition} *)

(** Minimal definition using [select] without {!module:Indexed_applicative}
    requirements. *)
module type WITH_SELECT = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Selective]. *)

  val select :
    (('a, 'b) Either.t, 'index) t -> ('a -> 'b, 'index) t -> ('b, 'index) t
  (** [select e f] apply [f] if [e] is [Left]. It allow to skip effect using
      [Right]. *)
end

(** Minimal definition using [branch] without {!module:Applicative}
    requirements. *)
module type WITH_BRANCH = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Selective]. *)

  val branch :
       (('a, 'b) Either.t, 'index) t
    -> ('a -> 'c, 'index) t
    -> ('b -> 'c, 'index) t
    -> ('c, 'index) t
  (** [branch] is like [select]. It chooses between two effects. *)
end

(** Standard requirement including [pure] and [select]. *)
module type WITH_PURE_AND_SELECT = sig
  include WITH_SELECT
  (** @inline *)

  val pure : 'a -> ('a, 'index) t
  (** Create a new [t]. *)
end

(** Standard requirement including [pure] and [branch]. *)
module type WITH_PURE_AND_BRANCH = sig
  include WITH_BRANCH
  (** @inline *)

  val pure : 'a -> ('a, 'index) t
  (** Create a new [t]. *)
end

(** {1 Structure anatomy} *)

(** Basis operation. *)
module type CORE = sig
  include WITH_SELECT
  (** @inline *)

  include WITH_BRANCH with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  include Indexed_applicative.CORE with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** Additional operations. *)
module type OPERATION = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Selective]. *)

  include
    Indexed_applicative.OPERATION with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  val if_ :
    (bool, 'index) t -> ('a, 'index) t -> ('a, 'index) t -> ('a, 'index) t
  (** Same of [branch] but using a [Boolean] as disjunction. *)

  val bind_bool : (bool, 'index) t -> (bool -> ('a, 'index) t) -> ('a, 'index) t
  (** {!module:Monad} [bind] specialized for Boolean. *)

  val when_ : (bool, 'index) t -> (unit, 'index) t -> (unit, 'index) t
  (** Conditionally perform an effect. *)

  val exists : ('a -> (bool, 'index) t) -> 'a list -> (bool, 'index) t
  (** Selective version of [List.exists]. *)

  val for_all : ('a -> (bool, 'index) t) -> 'a list -> (bool, 'index) t
  (** Selective version of [List.for_all]. *)

  val or_ : (bool, 'index) t -> (bool, 'index) t -> (bool, 'index) t
  (** Or combinator. *)

  val and_ : (bool, 'index) t -> (bool, 'index) t -> (bool, 'index) t
  (** And combinator. *)

  val while_ : (bool, 'index) t -> (unit, 'index) t
  (** Keep checking an effectful condition while it holds. *)
end

(** Syntax extensions. *)
module type SYNTAX = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Selective]. *)

  include Indexed_applicative.SYNTAX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end

(** Infix operators. *)
module type INFIX = sig
  type ('a, 'index) t
  (** The type held by the [Indexed Selective]. *)

  include Indexed_applicative.INFIX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  val ( <*? ) :
    (('a, 'b) Either.t, 'index) t -> ('a -> 'b, 'index) t -> ('b, 'index) t
  (** Infix version of {!val:CORE.select}. *)

  val ( <||> ) : (bool, 'index) t -> (bool, 'index) t -> (bool, 'index) t
  (** Infix version of {!val:CORE.or_}. *)

  val ( <&&> ) : (bool, 'index) t -> (bool, 'index) t -> (bool, 'index) t
  (** Infix version of {!val:CORE.and_}. *)
end

(** {1 Complete API} *)

(** The complete interface of an [Index Selective]. *)
module type API = sig
  (** {1 Type} *)

  type ('a, 'index) t
  (** The type held by the [Indexed Selective]. *)

  (** {1 Functions} *)

  include CORE with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  include OPERATION with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type ('a, 'index) t = ('a, 'index) t

  include INFIX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)

  (** {1 Syntax} *)

  module Syntax : SYNTAX with type ('a, 'index) t = ('a, 'index) t

  include SYNTAX with type ('a, 'index) t := ('a, 'index) t
  (** @inline *)
end
