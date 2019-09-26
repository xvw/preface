(** A [Selective] (applicative functor) allows to declare effects statically
    and select which execute dynamically. It is an algebraic structure between
    [Applicative] and [Monad].
*)

(** {1 Structure anatomy} *)

(** Standard requirement. *)
module type CORE = sig
  type 'a t

  include Applicative.CORE with type 'a t := 'a t

  val select : ('a, 'b) Either.t t -> ('a -> 'b) t -> 'b t
end

(** Operations. *)
module type OPERATION = sig
  type ('a, 'b) either

  type 'a t

  include Applicative.OPERATION with type 'a t := 'a t

  val branch : ('a, 'b) Either.t t -> ('a -> 'c) t -> ('b -> 'c) -> 'c t

  val if_ : bool t -> 'a t -> 'a t -> 'a t

  val when_ : bool t -> unit t -> unit t

  val while_ : bool t -> unit t
end

(** Syntax extensions. *)
module type SYNTAX = sig
  include Applicative.SYNTAX
end

(** Infix notations. *)
module type INFIX = sig
  type 'a t

  include Applicative.INFIX with type 'a t := 'a t

  val ( <*? ) : ('a, 'b) Either.t t -> ('a -> 'b) t -> 'b t

  val ( <||> ) : bool t -> bool t -> bool t

  val ( <&&> ) : bool t -> bool t -> bool t
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
