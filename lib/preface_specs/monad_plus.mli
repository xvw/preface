(** [Monad_plus] is a kind of {!module:Monoid} on {!module:Monad}. A
    [Monad_plus] is formally a {!module:Monad} with neutral and [combine]. So a
    [Monad_plus] is also a {!module:Monad}. *)

(** {2 Laws}

    + All alternatives laws *)

(** {1 Minimal definition} *)

(** Minimal interfaces of [Alternative] without {!module:Monad}. *)
module type WITH_NEUTRAL_AND_COMBINE = sig
  include Alternative.WITH_NEUTRAL_AND_COMBINE
  (** @closed *)
end

(** Minimal definition using [neutral], [combine], [return], [map] and [join]. *)
module type WITH_MAP_AND_JOIN = sig
  include Monad.WITH_MAP_AND_JOIN
  (** @closed *)

  include WITH_NEUTRAL_AND_COMBINE with type 'a t := 'a t
  (** @closed *)
end

(** Minimal definition using [neutral], [combine], [return],
    [compose_left_to_right]. *)
module type WITH_KLEISLI_COMPOSITION = sig
  include Monad.WITH_KLEISLI_COMPOSITION

  include WITH_NEUTRAL_AND_COMBINE with type 'a t := 'a t
end

(** Minimal definition using [neutral], [combine], [return], [bind]. *)
module type WITH_BIND = sig
  include Monad.WITH_BIND
  (** @closed *)

  include WITH_NEUTRAL_AND_COMBINE with type 'a t := 'a t
  (** @closed *)
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  include Monad.CORE
  (** @closed *)

  include WITH_NEUTRAL_AND_COMBINE with type 'a t := 'a t
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
