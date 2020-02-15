(** A {!Functor} parametrized by 2 types *)

(** Standard requirement. *)
module type CORE = sig
  type ('a, 'b) t
  (** The type holded by the [Functor2]. *)

  val map : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  (** Mapping over from ['b] to ['c] over [('a, 'b) t] to [('a, 'c) t]. *)
end

(** Operations *)
module type OPERATION = sig
  type ('a, 'b) t
  (** The type holded by the [Functor2]. *)

  val replace : 'c -> ('a, 'b) t -> ('a, 'c) t
  (** Create a new [('a, 'c) t], replacing all values in the [('a, 'b) t] by
      given a value of ['c]. *)

  val void : ('a, 'b) t -> ('a, unit) t
  (** Create a new [('a, unit) t], replacing all values in the [('a, 'b) t] by
      [unit]. *)
end

(** Infix notation *)
module type INFIX = sig
  type ('a, 'b) t
  (** The type holded by the [Functor2]. *)

  val ( <$> ) : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  (** Infix version of {!val:CORE.map}. *)

  val ( <&> ) : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
  (** Flipped and infix version of {!val:CORE.map}. *)

  val ( <$ ) : 'c -> ('a, 'b) t -> ('a, 'c) t
  (** Infix version of {!val:OPERATION.replace}. *)

  val ( $> ) : ('a, 'b) t -> 'c -> ('a, 'c) t
  (** Flipped and infix version of {!val:OPERATION.replace}. *)
end

(** {1 API} *)

(** The complete interface of a [Functor2]. *)
module type API = sig
  include CORE

  include OPERATION with type ('a, 'b) t := ('a, 'b) t

  module Infix : INFIX with type ('a, 'b) t := ('a, 'b) t

  include module type of Infix
end

(** {1 Bibliography}

    - {{:https://wiki.haskell.org/Functor} Haskell's documentation of a Functor} *)
