(** [Decidable] is a "Contravariant Alternative". *)

(** {2 Laws}

    To have a predictable behaviour, the instance of [Decidable] must obey some
    laws.

    + All {!module:Divisible} laws
    + [choose Either.left x (lose f) = x]
    + [choose Either.right x (lose f) = x]
    + {[
        choose f (choose g m n) o
        =
        let f' =
          Either.(case (case id (right % left) % g) (right % right) % right)
        in
        choose f' m (choose Fun.id n o)
      ]} *)

open Preface_core
open Shims

(** {1 Minimal definition} *)

(** Exposes the [lose] and [choose] functions, mandatory for each requirement. *)
module type WITH_LOSE_AND_CHOOSE = sig
  type 'a t
  (** The type held by the [Decidable]. *)

  val lose : ('a -> Void.t) -> 'a t
  (** Since [Void.t] is identity for [Either.t], [lose] act as an identity for
      Decidable. *)

  val choose : ('a -> ('b, 'c) Either.t) -> 'b t -> 'c t -> 'a t
  (** [choose f x y] says that if [f] can handle either [x] or [y] it can handle
      the result of the function. *)
end

module type WITH_CONTRAMAP_AND_DIVIDE_AND_CONQUER = sig
  include WITH_LOSE_AND_CHOOSE

  include Divisible.WITH_CONTRAMAP_AND_DIVIDE_AND_CONQUER with type 'a t := 'a t
end

module type CORE = WITH_CONTRAMAP_AND_DIVIDE_AND_CONQUER

module type OPERATION = sig
  type 'a t
  (** The type held by the [Decidable]. *)

  val lost : Void.t t
  (** [lose] acting on identity.*)

  val chosen : 'a t -> 'b t -> ('a, 'b) Either.t t
  (** [choose] acting on identity.*)

  include Divisible.OPERATION with type 'a t := 'a t
end

module type INFIX = sig
  type 'a t
  (** The type held by the [Decidable]. *)

  include Divisible.INFIX with type 'a t := 'a
end

(** {1 Complete API} *)

(** The complete interface of a [Decidable]. *)
module type API = sig
  (** {1 Type} *)
  type 'a t
  (** The type held by the [Decidable]. *)

  (** {1 Functions} *)

  include CORE with type 'a t := 'a t
  (** @inline *)

  include OPERATION with type 'a t := 'a t
  (** @inline *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type 'a t := 'a t

  include module type of Infix
  (** @inline *)
end

(** {1 Additional references}

    - {{:https://typeclasses.com/contravariance}
      https://typeclasses.com/contravariance} *)
