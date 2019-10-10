(** A [Monad] - TODO *)

(** {1 Structure anatomy} *)

(** Requirement via [bind]. *)
module type CORE_VIA_BIND = sig
  type 'a t
  (** The type holded by the [Monad]. *)

  val return : 'a -> 'a t
  (** Create a new ['a t]. *)

  val bind : ('a -> 'b t) -> 'a t -> 'b t
  (** [bind f m] passes the result of computation [m] to function [f]. *)
end

(** Requirement via [map] and [join]. *)
module type CORE_VIA_MAP_AND_JOIN = sig
  type 'a t
  (** The type holded by the [Monad]. *)

  val return : 'a -> 'a t
  (** Create a new ['a t]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** Mapping over from ['a] to ['b] over ['a t] to ['b t]. *)

  val join : 'a t t -> 'a t
  (** [join] remove one level of monadic structure, projecting
      its bound argument into the outer level.
  *)
end

(** Requirement via [compose_left_to_right]. *)
module type CORE_VIA_KLEISLI_COMPOSITION = sig
  type 'a t
  (** The type holded by the [Monad]. *)

  val return : 'a -> 'a t
  (** Create a new ['a t]. *)

  val compose_left_to_right : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t
  (** Composing monadic functions using Kleisli Arrow (from left to right).
  *)
end

(** Standard requirement. *)
module type CORE = sig
  include CORE_VIA_BIND

  include CORE_VIA_MAP_AND_JOIN with type 'a t := 'a t

  include CORE_VIA_KLEISLI_COMPOSITION with type 'a t := 'a t
end

(** Operations. *)
module type OPERATION = sig
  type 'a t
  (** The type holded by the [Monad]. *)

  val void : 'a t -> unit t
  (** Discard the result of evaluation. *)

  val compose_right_to_left : ('b -> 'c t) -> ('a -> 'b t) -> 'a -> 'c t
  (** Composing monadic functions using Kleisli Arrow (from right to left).
  *)

  val lift : ('a -> 'b) -> 'a t -> 'b t
  (** Mapping over from ['a] to ['b] over ['a t] to ['b t]. *)

  val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** Mapping over from ['a] and ['b] to ['c] over ['a t] and
      ['b t] to ['c t].
  *)

  val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  (** Mapping over from ['a] and ['b] and ['c] to ['d] over ['a t]
      and ['b t] and ['c t] to ['d t].
  *)
end

(** Syntax extensions. *)
module type SYNTAX = sig
  type 'a t
  (** The type holded by the [Monad]. *)

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  (** Syntaxic shortcuts for flipped version of {!val:CORE.bind}:

      [let* x = e in f] is equals to [bind (fun x -> f) e].
  *)
end

(** Infix notations. *)
module type INFIX = sig
  type 'a t
  (** The type holded by the [Monad]. *)

  val ( =|< ) : ('a -> 'b) -> 'a t -> 'b t
  (** Infix version of {!val:CORE.map}. *)

  val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
  (** Infix flipped version of {!val:CORE.map}. *)

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  (** Infix flipped version of {!val:CORE.bind}. *)

  val ( =<< ) : ('a -> 'b t) -> 'a t -> 'b t
  (** Infix version of {!val:CORE.bind}. *)

  val ( >=> ) : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t
  (** Infix version of {!val:CORE.compose_left_to_right}. *)

  val ( <=< ) : ('b -> 'c t) -> ('a -> 'b t) -> 'a -> 'c t
  (** Infix version of {!val:OPERATION.compose_right_to_left}. *)

  val ( >> ) : 'a t -> 'b t -> 'b t
  (** Sequentially compose two actions, discarding any value produced
      by the first.
  *)

  val ( << ) : 'a t -> 'b t -> 'a t
  (** Sequentially compose two actions, discarding any value produced
      by the second.
  *)
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

(** 
   {1 Bibliography}

   - {{: http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad.html} 
   Haskell's documentation of a Monad}
   - {{: http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf} Monad for functional programming}
   - {{: https://person.dibris.unige.it/moggi-eugenio/ftp/ic91.pdf} Notions of computations and monads}
   - {{: https://wiki.haskell.org/All_About_Monads} All About Monads}
*)
