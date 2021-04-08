(** [Alternative] is a kind of {!module:Monoid} on {!module:Applicative}. An
    [Alternative] is formally an {!module:Applicative} with [neutral] and
    [combine]. So an [Alternative] is also an {!module:Applicative} (and an
    {!module:Alt} which is also a {!module:Functor}).*)

(** {2 Laws}

    To have a predictable behaviour, the instance of [Alternative] must obey
    some laws.

    + All {!module:Applicative} laws
    + All {!module:Alt} laws
    + [combine x neutral = combine neutral x = x] *)

(** {1 Minimal definition} *)

(** Minimal interfaces of [Alternative] without {!module:Applicative}. *)
module type WITH_NEUTRAL_AND_COMBINE = sig
  include Alt.WITH_COMBINE
  (** @inline *)

  val neutral : 'a t
  (** The neutral element of the [Alternative]. *)
end

(** Minimal definition using [neutral], [combine], [pure], [map] and [product]. *)
module type WITH_MAP_AND_PRODUCT = sig
  include Applicative.WITH_MAP_AND_PRODUCT
  (** @inline *)

  include WITH_NEUTRAL_AND_COMBINE with type 'a t := 'a t
  (** @inline *)
end

(** Minimal definition using [neutral], [combine], [pure] and [apply]. *)
module type WITH_APPLY = sig
  include Applicative.WITH_APPLY

  include WITH_NEUTRAL_AND_COMBINE with type 'a t := 'a t
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  include WITH_NEUTRAL_AND_COMBINE
  (** @inline *)

  include Applicative.CORE with type 'a t := 'a t
  (** @inline *)
end

(** Operation without {!module:Applicative}. *)
module type ALTERNATIVE_OPERATION = sig
  include Alt.OPERATION
  (** @inline *)

  val reduce : 'a t list -> 'a t
  (** Reduce a [List.t] using [combine]. *)
end

(** Additional operations. *)
module type OPERATION = sig
  include Applicative.OPERATION
  (** @inline *)

  include ALTERNATIVE_OPERATION with type 'a t := 'a t
  (** @inline *)
end

(** Infix operators. *)
module type INFIX = sig
  include Applicative.INFIX
  (** @inline *)

  include Alt.INFIX with type 'a t := 'a t
  (** @inline *)
end

(** Syntax extensions. *)
module type SYNTAX = sig
  include Applicative.SYNTAX
  (** @inline *)
end

(** {1 Complete API} *)

(** The complete interface of an [Alternative]. *)
module type API = sig
  (** {1 Type} *)

  type 'a t
  (** The type held by the [Alternative]. *)

  (** {1 Functions} *)

  include CORE with type 'a t := 'a t
  (** @inline *)

  include OPERATION with type 'a t := 'a t
  (** @inline *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type 'a t := 'a t

  include module type of Infix
  (** @inline *)

  (** {1 Syntax} *)

  module Syntax : SYNTAX with type 'a t := 'a t

  include module type of Syntax
  (** @inline *)
end

(** {1 Additional references}

    - {{:https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Applicative.html#g:2}
      Haskell's documentation of an Alternative}
    - {{:https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus}
      Alternative and Monad plus on Haskell Wiki} *)
