(** [Alt] is a [Functor] which is a kind of [Semigroup] over a parametrized
    type. *)

(** {1 Structure anatomy} *)

(** Combine operation. *)
module type WITH_COMBINE = sig
  type 'a t
  (** A type ['a t] which is an [Alt]. *)

  val combine : 'a t -> 'a t -> 'a t
  (** Combine two values of ['a t] into one. *)
end

(** Standard requirement. *)
module type CORE = sig
  include WITH_COMBINE

  include Functor.CORE with type 'a t := 'a t
end

(** Operations *)
module type OPERATION = sig
  type 'a t
  (** A type [t] which is am [Alt]. *)

  val times : int -> 'a t -> 'a t option
  (** [times n x] apply [combine] on [x] [n] times. If [n] is lower than [1] the
      function will returns [None]. *)

  val reduce_nel : 'a t Preface_core.Nonempty_list.t -> 'a t
  (** Reduce a [Nonempty_list.t] using [combine]. *)

  include Functor.OPERATION with type 'a t := 'a t
end

(** Infix notation *)
module type INFIX = sig
  type 'a t
  (** A type ['a t] which is an [Alt]. *)

  val ( <|> ) : 'a t -> 'a t -> 'a t
  (** Infix version of {!val:CORE.combine} *)

  include Functor.INFIX with type 'a t := 'a t
end

(** The complete interface of an [Alt]. *)
module type API = sig
  include CORE

  include OPERATION with type 'a t := 'a t

  module Infix : INFIX with type 'a t := 'a t

  include INFIX with type 'a t := 'a t
end

(** {1 Bibliography}

    - {{:https://hackage.haskell.org/package/semigroupoids-5.3.4/docs/Data-Functor-Alt.html}
      Haskell's documentation of Alt} *)
