(** [Monad_plus] is a kind of {!module:Monoid} on {!module:Monad}. A
    [Monad_plus] is formally a {!module:Monad} with neutral and [combine]. So a
    [Monad_plus] is also a {!module:Monad}. *)

(** {2 Laws}

    + All alternatives laws *)

(** {1 Structure anatomy} *)

(** Additional operations over a [Monad]. *)
module type CORE_WITH_NEUTRAL_AND_COMBINE = sig
  include Alternative.CORE_WITH_NEUTRAL_AND_COMBINE
  (** @closed *)
end

(** Minimal definition using [map] and [join]. *)
module type CORE_WITH_MAP_AND_JOIN = sig
  include Monad.CORE_WITH_MAP_AND_JOIN
  (** @closed *)

  include CORE_WITH_NEUTRAL_AND_COMBINE with type 'a t := 'a t
  (** @closed *)
end

(** Minimal definition using [compose_left_to_right]. *)
module type CORE_WITH_KLEISLI_COMPOSITION = sig
  include Monad.CORE_WITH_KLEISLI_COMPOSITION

  include CORE_WITH_NEUTRAL_AND_COMBINE with type 'a t := 'a t
end

(** Minimal definition using [bind]. *)
module type CORE_WITH_BIND = sig
  include Monad.CORE_WITH_BIND
  (** @closed *)

  include CORE_WITH_NEUTRAL_AND_COMBINE with type 'a t := 'a t
  (** @closed *)
end

(** The minimum definition of a [Monad_plus]. It is by using the combinators of
    this module that the other combinators will be derived. *)
module type CORE = sig
  include Monad.CORE
  (** @closed *)

  include CORE_WITH_NEUTRAL_AND_COMBINE with type 'a t := 'a t
  (** @closed *)
end

(** Additional operations. *)
module type OPERATION = sig
  include Monad.OPERATION
  (** @closed *)

  include Alternative.ALTERNATIVE_OPERATION with type 'a t := 'a t
  (** @closed *)

  val filter : ('a -> bool) -> 'a t -> 'a t
  (** Filtering over [Monad_plus]. *)
end

(** Infix operators. *)
module type INFIX = sig
  include Monad.INFIX
  (** @closed *)

  val ( <|> ) : 'a t -> 'a t -> 'a t
  (** Infix version of {!val:CORE.combine}. *)
end

(** Syntax extensions *)
module type SYNTAX = sig
  include Monad.SYNTAX
  (** @closed *)
end

(** {1 Complete API} *)

(** The complete interface of a [Monad_plus]. *)
module type API = sig
  (** {1 Core functions}

      Set of fundamental functions in the description of a [Monad_plus]. *)

  include CORE
  (** @closed *)

  (** {1 Additional functions}

      Additional functions, derived from fundamental functions. *)

  include OPERATION with type 'a t := 'a t
  (** @closed *)

  (** {1 Syntax} *)

  module Syntax : SYNTAX with type 'a t := 'a t

  include module type of Syntax
  (** @closed *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type 'a t := 'a t

  (** {2 Infix operators inclusion} *)

  include module type of Infix
  (** @closed *)
end

(** {1 Additional references}

    - {{:https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Monad.html#t:MonadPlus}
      Haskell's documentation of a Monad plus}
    - {{:https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus}
      Alternative and Monad plus on Haskell Wiki} *)
