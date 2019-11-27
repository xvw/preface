(** Modules for building [FreeEr] modules. *)

module Make_freeEr_functor (F : sig
  type 'a t
end) : Preface_specs.FUNCTOR with type 'a t = 'a Preface_specs.FreeEr.CORE(F).t

module Make_freeEr_applicative (F : sig
  type 'a t
end) : Preface_specs.APPLICATIVE with type 'a t = 'a Preface_specs.FreeEr.CORE(F).t

module Make_freeEr_monad (F : sig
  type 'a t
end) : Preface_specs.MONAD with type 'a t = 'a Preface_specs.FreeEr.CORE(F).t

