(** [Invariant] is and "Invariant Functor". Every {!module:Functor} and
    {!module:Contravariant} is an Invariant Functor. *)

(** {2 Laws}

    To have a predictable behaviour, the instance of [Invariant] must obey some
    laws.

    + [invmap id id = id]
    + [invmap f2 f2' % invmap f1 f1' = invmap (f2 % f1) (f1' % f2')] *)

(** {1 Minimal definition} *)

module type WITH_INVMAP = sig
  type 'a t
  (** The type held by the [Invariant]. *)

  val invmap : ('a -> 'b) -> ('b -> 'a) -> 'a t -> 'b t
  (** Transform an ['a t] into an ['b t] given a transformation from ['a] to
      ['b] and one from ['b] to ['a]. *)
end

(** {1 Structure anatomy} *)

module type CORE = WITH_INVMAP

(** {1 Complete API} *)

module type API = CORE
(** The complete interface of an [Invariant]. *)

(** {1 Additional references}

    - {{:https://hackage.haskell.org/package/invariant} Haskell's documentation
      of Invariant Functor}
    - {{:http://comonad.com/reader/2008/rotten-bananas/} Rotten Bananas} *)
