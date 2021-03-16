(** [Alternative] is a kind of [Monoid] on [Applicative functors]. An
    [Alternative] is formally an [Applicative] with [neutral] and [combine]. *)

(** {1 Structure anatomy} *)

(** Additional operations over [Applicative]. *)
module type CORE_WITH_NEUTRAL_AND_COMBINE = sig
  include Alt.WITH_COMBINE

  val neutral : 'a t
  (** The neutral element of the [Alternative]. *)
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

(** Operation without Applicative *)
module type ALTERNATIVE_OPERATION = sig
  include Alt.OPERATION

  val reduce : 'a t list -> 'a t
  (** Reduce a [List.t] using [combine]. *)
end

(** Operations *)
module type OPERATION = sig
  include Applicative.OPERATION

  include ALTERNATIVE_OPERATION with type 'a t := 'a t
end

(** Infix notations *)
module type INFIX = sig
  include Applicative.INFIX

  include Alt.INFIX with type 'a t := 'a t
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

(** {1 Bibliography}

    - {{:https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Applicative.html#g:2}
      Haskell's documentation of an Alternative}
    - {{:https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus}
      Alternative and Monad plus on Haskell Wiki} *)
