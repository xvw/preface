(** [Contravariant] is a "Contravariant functor". *)

(** {1 Structure anatomy} *)

(** Standard requirement. *)
module type CORE = sig
  type 'a t
  (** The type held by the [Contravriant Functor]. *)

  val contramap : ('a -> 'b) -> 'b t -> 'a t
  (** Mapping over from ['a] to ['b] over ['b t] to ['a t]. *)
end

(** Operations *)
module type OPERATION = sig
  type 'a t
  (** The type held by the [Contravriant Functor]. *)

  val replace : 'b -> 'b t -> 'a t
  (** Replace all locations in the output with the same value. *)
end

(** Infix notation *)
module type INFIX = sig
  type 'a t
  (** The type held by the [Contravriant Functor]. *)

  val ( >$ ) : 'b -> 'b t -> 'a t
  (** Infix version of {!val:OPERATION.replace}. *)

  val ( $< ) : 'b t -> 'b -> 'a t
  (** Infix flipped version of {!val:OPERATION.replace}. *)

  val ( >$< ) : ('a -> 'b) -> 'b t -> 'a t
  (** Infix version of {!val:CORE.map}. *)

  val ( >&< ) : 'b t -> ('a -> 'b) -> 'a t
  (** Infix flipped version of {!val:CORE.map}. *)
end

(** {1 API} *)

(** The complete interface of a [Contravariant Functor]. *)
module type API = sig
  include CORE

  include OPERATION with type 'a t := 'a t

  module Infix : INFIX with type 'a t := 'a t

  include module type of Infix
end

(** {1 Bibliography}

    - {{:https://typeclasses.com/contravariance}
      https://typeclasses.com/contravariance} *)
