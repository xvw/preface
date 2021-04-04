(** [Alt] is a {!module:Functor} which is a kind of {!module:Semigroup} over a
    parametrized type. In other word, [Alt] is a {!module:Functor} with a
    [combine] operation. *)

(** {2 Laws}

    To ensure that the derived combiners work properly, an [Alt] should respect
    these laws:

    + [combine (combine a b) c = combine a (combine b c)]
    + [map f (combine a b) = combine (map f a) (map f b)] *)

(** {1 Structure anatomy} *)

(** Combine operation. This signature is mainly used to enrich a
    {!module:Functor} with [combine].*)
module type WITH_COMBINE = sig
  type 'a t
  (** A type ['a t] held by the [Alt]. *)

  val combine : 'a t -> 'a t -> 'a t
  (** Combine two values of ['a t] into one. *)
end

(** The minimum definition of an [Alt]. It is by using the combinators of this
    module that the other combinators will be derived. *)
module type CORE = sig
  include WITH_COMBINE

  include Functor.CORE with type 'a t := 'a t
  (** @closed *)
end

(** Additional operations. *)
module type OPERATION = sig
  type 'a t
  (** A type ['a t] held by the [Alt]. *)

  val times : int -> 'a t -> 'a t option
  (** [times n x] apply [combine] on [x] [n] times. If [n] is lower than [1] the
      function will returns [None]. *)

  val reduce_nel : 'a t Preface_core.Nonempty_list.t -> 'a t
  (** Reduce a [Nonempty_list.t] using [combine]. *)

  include Functor.OPERATION with type 'a t := 'a t
  (** @closed *)
end

(** Infix operators. *)
module type INFIX = sig
  type 'a t
  (** A type ['a t] which is an [Alt]. *)

  val ( <|> ) : 'a t -> 'a t -> 'a t
  (** Infix version of {!val:CORE.combine} *)

  include Functor.INFIX with type 'a t := 'a t
  (** @closed *)
end

(** {1 Complete API} *)

(** The complete interface of an [Alt]. *)
module type API = sig
  (** {1 Core functions}

      Set of fundamental functions in the description of an [Alt]. *)

  include CORE
  (** @closed *)

  (** {1 Additional functions}

      Additional functions, derived from fundamental functions. *)

  include OPERATION with type 'a t := 'a t
  (** @closed *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type 'a t := 'a t

  (** {2 Infix operators inclusion} *)

  include INFIX with type 'a t := 'a t
  (** @closed *)
end

(** {1 Additional references}

    - {{:https://hackage.haskell.org/package/semigroupoids-5.3.4/docs/Data-Functor-Alt.html}
      Haskell's documentation of Alt} *)
