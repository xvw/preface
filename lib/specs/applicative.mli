(** An [Applicative] for [('a -> 'b) t] is TODO.
*)

(** {1 Structure anatomy} *)

(** Standard requirement. *)
module type CORE = sig
  type 'a t
  (** The type holded by the [Applicative]. *)

  val pure : 'a -> 'a t
  (** Create a new ['a t]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** Mapping over from ['a] to ['b] over ['a t] to ['b t]. *)

  val apply : ('a -> 'b) t -> 'a t -> 'b t
  (** TODO *)

  val product : 'a t -> 'b t -> ('a * 'b) t
  (** TODO *)
end

(** Standard requirement . *)
module type CORE_VIA_MAP_AND_PRODUCT = sig
  type 'a t
  (** The type holded by the [Applicative]. *)

  val pure : 'a -> 'a t
  (** Create a new ['a t]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** Mapping over from ['a] to ['b] over ['a t] to ['b t]. *)

  val product : 'a t -> 'b t -> ('a * 'b) t
  (** TODO *)
end

(** Standard requirement. *)
module type CORE_VIA_APPLY = sig
  type 'a t
  (** The type holded by the [Applicative]. *)

  val pure : 'a -> 'a t
  (** Create a new ['a t]. *)

  val apply : ('a -> 'b) t -> 'a t -> 'b t
  (** TODO *)
end

(** Operations *)
module type OPERATION = sig
  type 'a t
  (** The type holded by the [Applicative]. *)

  val liftA : ('a -> 'b) -> 'a t -> 'b t
  (** TODO *)

  val liftA2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** TODO *)

  val liftA3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  (** TODO *)
end

(** Syntax extensions *)
module type SYNTAX = sig
  type 'a t
  (** The type holded by the [Applicative]. *)

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  (** TODO *)

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  (** TODO *)
end

(** Infix notations *)
module type INFIX = sig
  type 'a t
  (** The type holded by the [Applicative]. *)

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  (** TODO *)

  val ( <**> ) : 'a t -> ('a -> 'b) t -> 'b t
  (** TODO *)

  val ( <* ) : 'a t -> 'b t -> 'a t
  (** TODO *)

  val ( *> ) : 'a t -> 'b t -> 'b t
  (** TODO *)
end

(** {1 API} *)

(** The complete interface of a [Functor]. *)
module type API = sig
  include CORE

  include OPERATION with type 'a t := 'a t

  module Syntax : SYNTAX with type 'a t := 'a t

  include module type of Syntax

  module Infix : INFIX with type 'a t := 'a t

  include module type of Infix
end
