(** [Monad_plus] is a kind of [Monoid] on [Monad]. A [Monad_plus] is formally a
    [Monad] with neutral and [combine]. *)

(** {1 Structure anatomy} *)

module type CORE_WITH_NEUTRAL_AND_COMBINE =
  Alternative.CORE_WITH_NEUTRAL_AND_COMBINE
(** Additional operations over [Monad]. *)

(** Requirement via [map] and [join]. *)
module type CORE_WITH_MAP_AND_JOIN = sig
  include Monad.CORE_WITH_MAP_AND_JOIN

  include CORE_WITH_NEUTRAL_AND_COMBINE with type 'a t := 'a t
end

(** Requirement via [compose_left_to_right]. *)
module type CORE_WITH_KLEISLI_COMPOSITION = sig
  include Monad.CORE_WITH_KLEISLI_COMPOSITION

  include CORE_WITH_NEUTRAL_AND_COMBINE with type 'a t := 'a t
end

(** Requirement via [bind]. *)
module type CORE_WITH_BIND = sig
  include Monad.CORE_WITH_BIND

  include CORE_WITH_NEUTRAL_AND_COMBINE with type 'a t := 'a t
end

(** Standard requirement. *)
module type CORE = sig
  include Monad.CORE

  include CORE_WITH_NEUTRAL_AND_COMBINE with type 'a t := 'a t
end

(** Operations *)
module type OPERATION = sig
  include Monad.OPERATION

  val filter : ('a -> bool) -> 'a t -> 'a t
  (** Filtering over [Monad_plus]. *)
end

(** Infix notations *)
module type INFIX = sig
  include Monad.INFIX

  val ( <|> ) : 'a t -> 'a t -> 'a t
  (** Infix version of {!val:CORE.combine}. *)
end

module type SYNTAX = Monad.SYNTAX
(** Syntax extensions *)

(** {1 API} *)

(** The complete interface of a [Monad_plus]. *)
module type API = sig
  include CORE

  include OPERATION with type 'a t := 'a t

  module Syntax : SYNTAX with type 'a t := 'a t

  include module type of Syntax

  module Infix : INFIX with type 'a t := 'a t

  include module type of Infix
end

(** {1 Bibliography}

    - {{:https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Monad.html#t:MonadPlus}
      Haskell's documentation of a Monad plus}
    - {{:https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus}
      Alternative and Monad plus on Haskell Wiki} *)
