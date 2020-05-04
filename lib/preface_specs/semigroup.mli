(** A [Semigroup] is a type [t] which provides an associative operation [concat]
    which lets you combine any two values of [t] into one. *)

(** {1 Structure anatomy} *)

(** Standard requirement *)
module type CORE = sig
  type t
  (** A type [t] which is a [Semigroup]. *)

  val concat : t -> t -> t
  (** Combine two values of [t] into one. *)
end

(** Operations *)
module type OPERATION = sig
  type t
  (** A type [t] which is a [Semigroup]. *)

  val times : int -> t -> t option
  (** [times n x] apply [concat] on [x] [n] times. If [n] is lower than [1] the
      function will returns [None]. *)

  val reduce : t Preface_core.Nonempty_list.t -> t
  (** Reduce a [Nonempty_list.t] using [concat]. *)
end

(** Infix notation *)
module type INFIX = sig
  type t
  (** A type [t] which is a [Semigroup]. *)

  val ( ++ ) : t -> t -> t
  (** Infix version of {!val:CORE.concat} *)
end

(** The complete interface of a [Semigroup]. *)
module type API = sig
  include CORE

  include OPERATION with type t := t

  module Infix : INFIX with type t = t

  include INFIX with type t := t
end

(** {1 Bibliography}

    - {{:http://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Semigroup.html}
      Haskell's documentation of a Semigroup} *)
