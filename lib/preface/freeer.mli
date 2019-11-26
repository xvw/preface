(** Modules for building [Freeer] modules. *)

module Make_freeer_functor (F : sig
  type 'a t
end) : Specs.FUNCTOR with type 'a t = 'a Specs.Freeer.CORE(F).t

module Make_freeer_applicative (F : sig
  type 'a t
end) : Specs.APPLICATIVE with type 'a t = 'a Specs.Freeer.CORE(F).t

module Make_freeer_monad (F : sig
  type 'a t
end) : Specs.MONAD with type 'a t = 'a Specs.Freeer.CORE(F).t

