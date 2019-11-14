(** A [Comonad] - TODO *)

(** {1 Structure anatomy} *)

(** Requirement via [bind]. *)
module type CORE_VIA_MAP_AND_DUPLICATE = sig
  type 'a t
  (** The type holded by the [Comonad]. *)

  val extract : 'a t -> 'a
  (** Extract a ['a] from  ['a t]. Dual of return*)

  val duplicate : 'a t -> 'a t t
  (** Dual of join. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** Mapping over from ['a] to ['b] over ['a t] to ['b t]. *)
end

(** Requirement via [map] and [join]. *)
module type CORE_VIA_EXTEND = sig
  type 'a t
  (** The type holded by the [Comonad]. *)

  val extract : 'a t -> 'a
  (** Extract a ['a] from  ['a t]. *)

  val extend : ('a t -> 'b) -> 'a t -> 'b t
  (** Dual of bind. *)
end

(** Requirement via [co_compose_left_to_right]. *)
module type CORE_VIA_COKLEISLI_COMPOSITION = sig
  type 'a t
  (** The type holded by the [Comonad]. *)

  val extract : 'a t -> 'a
  (** Extract a ['a] from  ['a t]. *)

  val compose_left_to_right : ('a t -> 'b) -> ('b t -> 'c) -> 'a t -> 'c
  (** Composing monadic functions using Co-Kleisli Arrow (from left to right). *)
end

(** Standard requirement. *)
module type CORE = sig
  include CORE_VIA_MAP_AND_DUPLICATE

  include CORE_VIA_EXTEND with type 'a t := 'a t

  include CORE_VIA_COKLEISLI_COMPOSITION with type 'a t := 'a t
end

(** Operations. *)
module type OPERATION = sig
  type 'a t
  (** The type holded by the [Comonad]. *)

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

  val compose_right_to_left : ('b t -> 'c) -> ('a t -> 'b) -> 'a t -> 'c
  (** Composing comonadic functions using Co-Kleisli Arrow (from right to left). *)

  val fix : ('a t -> 'a) t -> 'a
  (** Fix point for [Comonad] *)
end

(** Syntax *)
module type SYNTAX = sig
  type 'a t
  (** The type holded by the [Comonad]. *)

  val ( let@ ) : 'a t -> ('a t -> 'b) -> 'b t
  (** Syntaxic shortcuts for version of {!val:CORE.extend}:

      [let@ x = e in f] is equals to [extend f e].
  *)
end

(** Infix notations. *)
module type INFIX = sig
  type 'a t
  (** The type holded by the [Comonad]. *)

  val ( =>> ) : 'a t -> ('a t -> 'b) -> 'b t
  (** Infix flipped version of {!val:CORE.extend}. *)

  val ( <<= ) : ('a t -> 'b) -> 'a t -> 'b t
  (** Infix version of {!val:CORE.extend}. *)

  val ( =>= ) : ('a t -> 'b) -> ('b t -> 'c) -> 'a t -> 'c
  (** Infix version of {!val:CORE.compose_left_to_right}. *)

  val ( =<= ) : ('b t -> 'c) -> ('a t -> 'b) -> 'a t -> 'c
  (** Infix version of {!val:OPERATION.compose_right_to_left}. *)

  val ( <@@> ) : 'a t -> ('a -> 'b) t -> 'b t
  (** Applicative functor of [('a -> 'b) t] over ['a t] to ['b t]. *)

  val ( <@> ) : ('a -> 'b) t -> 'a t -> 'b t
  (** Applicative functor of [('a -> 'b) t] over ['a t] to ['b t]. *)

  val ( @> ) : 'a t -> 'b t -> 'b t
  (** Discard the value of the first argument. *)

  val ( <@ ) : 'a t -> 'b t -> 'a t
  (** Discard the value of the second argument. *)
end

(** {1 API} *)

(** The complete interface of a [Comonad]. *)
module type API = sig
  include CORE

  include OPERATION with type 'a t := 'a t

  module Syntax : SYNTAX with type 'a t := 'a t

  include module type of Syntax

  module Infix : INFIX with type 'a t := 'a t

  include module type of Infix
end