(** Modules for building [Free] modules. *)

module Make_free_functor (F : Preface_specs.Functor.CORE) :
  Preface_specs.FUNCTOR with type 'a t = 'a Preface_specs.Free.CORE(F).t

module Make_free_applicative (F : Preface_specs.Functor.CORE) :
  Preface_specs.APPLICATIVE with type 'a t = 'a Preface_specs.Free.CORE(F).t

module Make_free_monad (F : Preface_specs.Functor.CORE) :
  Preface_specs.MONAD with type 'a t = 'a Preface_specs.Free.CORE(F).t