(** [Alternative] is a kind of [Monoid] on [Applicative functors]. An
    [Alternative] is formally an [Applicative] with [neutral] and [combine]. *)

(** {1 Structure anatomy} *)

(** Additional operations over [Applicative]. *)
module type CORE_WITH_NEUTRAL_AND_COMBINE = sig
  type 'a t
  (** The type held by the [Alternative]. *)

  val neutral : 'a t
  (** The neutral element of the [Alternative]. *)

  val combine : 'a t -> 'a t -> 'a t
  (** Combine two values of ['a t] into one. *)
end

(** Requirement via [map] and [product]. *)
module type CORE_WITH_MAP_AND_PRODUCT = sig
  include Applicative.CORE_WITH_MAP_AND_PRODUCT

  include CORE_WITH_NEUTRAL_AND_COMBINE with type 'a t := 'a t
end

(** Requirement via [apply]. *)
module type CORE_WITH_APPLY = sig
  include Applicative.CORE_WITH_APPLY

  include CORE_WITH_NEUTRAL_AND_COMBINE with type 'a t := 'a t
end

(** Standard requirement. *)
module type CORE = sig
  include CORE_WITH_APPLY

  include CORE_WITH_MAP_AND_PRODUCT with type 'a t := 'a t
end

module type OPERATION = Applicative.OPERATION
(** Operations *)

(** Infix notations *)
module type INFIX = sig
  include Applicative.INFIX

  val ( <|> ) : 'a t -> 'a t -> 'a t
  (** Infix version of {!val:CORE.combine}. *)
end

module type SYNTAX = Applicative.SYNTAX
(** Syntax extensions *)

(** {1 API} *)

(** The complete interface of an [Alternative]. *)
module type API = sig
  include CORE

  include OPERATION with type 'a t := 'a t

  module Syntax : SYNTAX with type 'a t := 'a t

  include module type of Syntax

  module Infix : INFIX with type 'a t := 'a t

  include module type of Infix
end
