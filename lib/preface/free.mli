(** Modules for building [Free] modules. *)

module Make_free_functor (F : Specs.Functor.CORE) : Specs.Functor.CORE

module Make_free_applicative (F : Specs.Functor.CORE) : Specs.Applicative.CORE_VIA_APPLY