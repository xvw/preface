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

(** {1 Structure anatomy} *)

(** Additional operations over [Alternative]. *)
module type CORE_WITH_NEUTRAL_AND_COMBINE = sig
  include Alt.WITH_COMBINE
  (** @closed *)

  val neutral : 'a t
  (** The neutral element of the [Alternative]. *)
end

(** Minimal definition using [map] and [product]. *)
module type CORE_WITH_MAP_AND_PRODUCT = sig
  include Applicative.CORE_WITH_MAP_AND_PRODUCT
  (** @closed *)

  include CORE_WITH_NEUTRAL_AND_COMBINE with type 'a t := 'a t
  (** @closed *)
end

(** Minimal definition using [apply]. *)
module type CORE_WITH_APPLY = sig
  include Applicative.CORE_WITH_APPLY

  include CORE_WITH_NEUTRAL_AND_COMBINE with type 'a t := 'a t
end

(** The minimum definition of an [Alternative]. It is by using the combinators
    of this module that the other combinators will be derived. *)
module type CORE = sig
  include CORE_WITH_APPLY
  (** @closed *)

  include CORE_WITH_MAP_AND_PRODUCT with type 'a t := 'a t
  (** @closed *)
end

(** Operation without {!module:Applicative}. *)
module type ALTERNATIVE_OPERATION = sig
  include Alt.OPERATION
  (** @closed *)

  val reduce : 'a t list -> 'a t
  (** Reduce a [List.t] using [combine]. *)
end

(** Additional operations. *)
module type OPERATION = sig
  include Applicative.OPERATION
  (** @closed *)

  include ALTERNATIVE_OPERATION with type 'a t := 'a t
  (** @closed *)
end

(** Infix operators. *)
module type INFIX = sig
  include Applicative.INFIX
  (** @closed *)

  include Alt.INFIX with type 'a t := 'a t
  (** @closed *)
end

(** Syntax extensions. *)
module type SYNTAX = sig
  include Applicative.SYNTAX
  (** @closed *)
end

(** {1 Complete API} *)

(** The complete interface of an [Alternative]. *)
module type API = sig
  (** {1 Core functions}

      Set of fundamental functions in the description of an [Alternative]. *)

  include CORE
  (** @closed *)

  (** {1 Additional functions}

      Additional functions, derived from fundamental functions. *)

  include OPERATION with type 'a t := 'a t
  (** @closed *)

  (** {1 Syntax} *)

  module Syntax : SYNTAX with type 'a t := 'a t

  (** {2 Syntax inclusion} *)

  include module type of Syntax
  (** @closed *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type 'a t := 'a t

  (** {2 Infix operators inclusion} *)

  include module type of Infix
  (** @closed *)
end

(** {1 Additional references}

    - {{:https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Applicative.html#g:2}
      Haskell's documentation of an Alternative}
    - {{:https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus}
      Alternative and Monad plus on Haskell Wiki} *)
