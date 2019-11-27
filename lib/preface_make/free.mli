(** Modules for building [Free] modules. *)

module Make_free_functor (F : Specs.Functor.CORE) :
  Specs.FUNCTOR with type 'a t = 'a Specs.Free.CORE(F).t

module Make_free_applicative (F : Specs.Functor.CORE) :
  Specs.APPLICATIVE with type 'a t = 'a Specs.Free.CORE(F).t

module Make_free_monad (F : Specs.Functor.CORE) :
  Specs.MONAD with type 'a t = 'a Specs.Free.CORE(F).t