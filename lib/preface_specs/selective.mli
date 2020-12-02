(** A [Selective] (applicative functor) allows to declare effects statically and
    select which execute dynamically. It is an algebraic structure between
    [Applicative] and [Monad]. *)

(** {1 Structure anatomy} *)

module type CORE_WITH_SELECT = sig
  type 'a t
  (** The type held by the [Selective]. *)

  type ('a, 'b) either
  (** Representing the selective disjunction. *)

  val pure : 'a -> 'a t
  (** Create a new ['a t]. *)

  val select : ('a, 'b) either t -> ('a -> 'b) t -> 'b t
  (** [select e f] apply [f] if [e] is [Left]. It allow to skip effect using
      [Right]. *)
end

(** Standard requirement. *)
module type CORE = sig
  include CORE_WITH_SELECT

  include Applicative.CORE with type 'a t := 'a t
  (** Each [Selective] is also an [Applicative]. *)
end

(** Operations. *)
module type OPERATION = sig
  type 'a t
  (** The type held by the [Selective]. *)

  type ('a, 'b) either
  (** Representing the selective disjunction. *)

  include Applicative.OPERATION with type 'a t := 'a t
  (** Each [Selective] is also an [Applicative]. *)

  val branch : ('a, 'b) either t -> ('a -> 'c) t -> ('b -> 'c) t -> 'c t
  (** [branch] is like [select]. It chooses between two effects. *)

  val if_ : bool t -> 'a t -> 'a t -> 'a t
  (** Same of [branch] but using a [Boolean] as disjunction. *)

  val bind_bool : bool t -> (bool -> 'a t) -> 'a t
  (** [Monad.bin] specialized for Boolean. *)

  val when_ : bool t -> unit t -> unit t
  (** Conditionnally perform an effect. *)

  val exists : ('a -> bool t) -> 'a list -> bool t
  (** Selective version of [List.exists]. *)

  val for_all : ('a -> bool t) -> 'a list -> bool t
  (** Selective version of [List.for_all]. *)

  val or_ : bool t -> bool t -> bool t
  (** Or combinator. *)

  val and_ : bool t -> bool t -> bool t
  (** And combinator. *)

  val while_ : bool t -> unit t
  (** Keep checking an effectful condition while it holds. *)
end

module type LIFT = Applicative.LIFT
(** Lift operation *)

(** Syntax extensions. *)
module type SYNTAX = sig
  include Applicative.SYNTAX
end

(** Infix notations. *)
module type INFIX = sig
  type 'a t
  (** The type held by the [Selective]. *)

  type ('a, 'b) either
  (** Representing the selective disjunction. *)

  include Applicative.INFIX with type 'a t := 'a t
  (** Each [Selective] is also an [Applicative]. *)

  val ( <*? ) : ('a, 'b) either t -> ('a -> 'b) t -> 'b t
  (** Infix version of {!val:CORE.select}. *)

  val ( <||> ) : bool t -> bool t -> bool t
  (** Infix version of {!val:CORE.or_}. *)

  val ( <&&> ) : bool t -> bool t -> bool t
  (** Infix version of {!val:CORE.and_}. *)
end

(** {1 API} *)

(** The complete interface of a [Monad]. *)
module type API = sig
  include CORE

  include
    OPERATION with type 'a t := 'a t and type ('a, 'b) either := ('a, 'b) either

  include LIFT with type 'a t := 'a t

  module Syntax : SYNTAX with type 'a t := 'a t

  include module type of Syntax

  module Infix : INFIX with type 'a t := 'a t

  include module type of Infix with type ('a, 'b) either := ('a, 'b) either
end

(** {1 Bibliography}

    - {{:http://hackage.haskell.org/package/selective} Haskell's documentation
      of a Selective Application Functor}
    - {{:https://www.staff.ncl.ac.uk/andrey.mokhov/selective-functors.pdf}
      Selective Applicative Functors} *)
