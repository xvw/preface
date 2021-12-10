(** [Divisible] is a "Contravariant Applicative Functor", in other word,
    [Divisible] is the dual of an {!module:Applicative} *)

(** {2 Laws}

    To have a predictable behaviour, the instance of [Divisible] must obey some
    laws.

    + All {!module:Contravariant} laws
    + [divide f x conquer = contramap (Stdlib.fst % f) x]
    + [divide f conquer x = contramap (Stdlib.snd % f) x]
    + {[
        divide f (divide g m n) o
        = divide
            (fun x ->
              let (bc, _) = f x in
              let (b, c) = g bc in
              (a, (b, c)))
            m (divide id n o)
      ]} *)

(** {1 Minimal definition} *)

(** Exposes the [divide] and [conquer] functions, mandatory for each
    requirement. *)
module type WITH_DIVIDE_AND_CONQUER = sig
  type 'a t
  (** The type held by the [Divisible]. *)

  val divide : ('a -> 'b * 'c) -> 'b t -> 'c t -> 'a t
  (** The contravariant version of [apply] from [Applicative]. *)

  val conquer : 'a t
  (** Provide and empty value. *)
end

module type WITH_CONTRAMAP_AND_DIVIDE_AND_CONQUER = sig
  include WITH_DIVIDE_AND_CONQUER

  include Contravariant.WITH_CONTRAMAP with type 'a t := 'a t
end

(** {1 Structure anatomy} *)

module type CORE = WITH_CONTRAMAP_AND_DIVIDE_AND_CONQUER
(** Basis operations.*)

(** Additional operations. *)
module type OPERATION = sig
  type 'a t
  (** The type held by the [Divisible]. *)

  val divided : 'a t -> 'b t -> ('a * 'b) t

  val conquered : unit t

  include Contravariant.OPERATION with type 'a t := 'a t
end

(** Infix operators. *)
module type INFIX = sig
  type 'a t
  (** The type held by the [Divisible]. *)

  val ( >*< ) : 'a t -> 'b t -> ('a * 'b) t
  (** Infix version of [divided]. *)

  val ( >* ) : 'a t -> unit t -> 'a t
  (** Discard the value of the second argument. *)

  val ( *< ) : unit t -> 'a t -> 'a t
  (** Discard the value of the first argument. *)

  include Contravariant.INFIX with type 'a t := 'a t
end

(** {1 Complete API} *)

(** The complete interface of a [Divisible]. *)
module type API = sig
  (** {1 Type} *)
  type 'a t
  (** The type held by the [Divisible]. *)

  (** {1 Functions} *)

  include CORE with type 'a t := 'a t
  (** @inline *)

  include OPERATION with type 'a t := 'a t
  (** @inline *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type 'a t = 'a t

  include INFIX with type 'a t := 'a t
  (** @inline *)
end

(** {1 Additional references}

    - {{:https://typeclasses.com/contravariance}
      https://typeclasses.com/contravariance} *)
