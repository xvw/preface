(** A {!Functor} parametrized by 3 types *)

(** {1 Structure anatomy} *)

(** Standard requirement. *)
module type CORE = sig
  type ('a, 'b, 'c) t
  (** The type holded by the [Functor3]. *)

  val map : ('c -> 'd) -> ('a, 'b, 'c) t -> ('a, 'b, 'd) t
  (** Mapping over from ['c] to ['d] over [('a, 'b, 'c) t] to [('a, 'b, 'd) t]. *)
end

(** Operations *)
module type OPERATION = sig
  type ('a, 'b, 'c) t
  (** The type holded by the [Functor3]. *)

  val replace : 'd -> ('a, 'b, 'c) t -> ('a, 'b, 'd) t
  (** Create a new [('a, 'b, 'd) t], replacing all values in the
      [('a, 'b, 'c) t] by given a value of ['d]. *)

  val void : ('a, 'b, 'c) t -> ('a, 'b, unit) t
  (** Create a new [('a, 'b, unit) t], replacing all values in the
      [('a, 'b, 'c) t] by [unit]. *)
end

(** Infix notation *)
module type INFIX = sig
  type ('a, 'b, 'c) t
  (** The type holded by the [Functor3]. *)

  val ( <$> ) : ('c -> 'd) -> ('a, 'b, 'c) t -> ('a, 'b, 'd) t
  (** Infix version of {!val:CORE.map}. *)

  val ( <&> ) : ('a, 'b, 'c) t -> ('c -> 'd) -> ('a, 'b, 'd) t
  (** Flipped and infix version of {!val:CORE.map}. *)

  val ( <$ ) : 'd -> ('a, 'b, 'c) t -> ('a, 'b, 'd) t
  (** Infix version of {!val:OPERATION.replace}. *)

  val ( $> ) : ('a, 'b, 'c) t -> 'd -> ('a, 'b, 'd) t
  (** Flipped and infix version of {!val:OPERATION.replace}. *)
end

(** {1 API} *)

(** The complete interface of a [Functor3]. *)
module type API = sig
  include CORE

  include OPERATION with type ('a, 'b, 'c) t := ('a, 'b, 'c) t

  module Infix : INFIX with type ('a, 'b, 'c) t := ('a, 'b, 'c) t

  include module type of Infix
end

(** {1 Bibliography}

    - {{:https://wiki.haskell.org/Functor} Haskell's documentation of a Functor} *)
