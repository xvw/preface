(** Modules for building [Free] modules. *)

module Make_free_functor (F : Specs.Functor.CORE) :
  Specs.Functor.CORE with type 'a t = 'a Specs.Free.CORE(F).t

module Make_free_applicative (F : Specs.Functor.CORE) :
  Specs.Applicative.CORE with type 'a t = 'a Specs.Free.CORE(F).t

module Make_free_monad (F : Specs.Functor.CORE) :
  Specs.Monad.CORE with type 'a t = 'a Specs.Free.CORE(F).t