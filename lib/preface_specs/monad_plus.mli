(** [Monad_plus] is a kind of {!module:Monoid} on {!module:Monad}. A
    [Monad_plus] is formally a {!module:Monad} with neutral and [combine]. So a
    [Monad_plus] is also a {!module:Monad}. *)

(** {2 Laws}

    + All alternatives laws *)

(** {1 Minimal definition} *)

(** Minimal interfaces of [Alternative] without {!module:Monad}. *)
module type WITH_NEUTRAL_AND_COMBINE = sig
  type 'a t
  (** The type held by the [Monad_plus]. *)

  include
    Indexed_monad_plus.WITH_NEUTRAL_AND_COMBINE with type ('a, _) t := 'a t
  (** @inline *)
end

(** Minimal definition using [neutral], [combine], [return], [map] and [join]. *)
module type WITH_MAP_AND_JOIN = sig
  type 'a t
  (** The type held by the [Monad_plus]. *)

  include Indexed_monad_plus.WITH_MAP_AND_JOIN with type ('a, _) t := 'a t
  (** @inline *)
end

(** Minimal definition using [neutral], [combine], [return],
    [compose_left_to_right]. *)
module type WITH_KLEISLI_COMPOSITION = sig
  type 'a t
  (** The type held by the [Monad_plus]. *)

  include
    Indexed_monad_plus.WITH_KLEISLI_COMPOSITION with type ('a, _) t := 'a t
  (** @inline *)
end

(** Minimal definition using [neutral], [combine], [return], [bind]. *)
module type WITH_BIND = sig
  type 'a t
  (** The type held by the [Monad_plus]. *)

  include Indexed_monad_plus.WITH_BIND with type ('a, _) t := 'a t
  (** @inline *)
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  type 'a t
  (** The type held by the [Monad_plus]. *)

  include Indexed_monad_plus.CORE with type ('a, _) t := 'a t
  (** @inline *)
end

(** Additional operations. *)
module type OPERATION = sig
  type 'a t
  (** The type held by the [Monad_plus]. *)

  include Indexed_monad_plus.OPERATION with type ('a, _) t := 'a t
  (** @inline *)
end

(** Infix operators. *)
module type INFIX = sig
  type 'a t
  (** The type held by the [Monad_plus]. *)

  include Indexed_monad_plus.INFIX with type ('a, _) t := 'a t
  (** @inline *)
end

(** Syntax extensions *)
module type SYNTAX = sig
  type 'a t
  (** The type held by the [Monad_plus]. *)

  include Indexed_monad_plus.SYNTAX with type ('a, _) t := 'a t
  (** @inline *)
end

(** {1 Complete API} *)

(** The complete interface of a [Monad_plus]. *)
module type API = sig
  (** {1 Type} *)

  type 'a t
  (** The type held by the [Monad_plus]. *)

  include Indexed_monad_plus.API with type ('a, _) t := 'a t
  (** @inline *)
end

(** {1 Additional references}

    - {{:https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Monad.html#t:MonadPlus}
      Haskell's documentation of a Monad plus}
    - {{:https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus}
      Alternative and Monad plus on Haskell Wiki} *)
