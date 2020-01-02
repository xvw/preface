(** Modules for building [FreeEr] modules. *)

module Via (F : sig
  type 'a data
end) : Preface_specs.FREEER with type 'a data = 'a F.data

module Via_functor (CORE : Preface_specs.FREEER) :
  Preface_specs.FUNCTOR with type 'a t = 'a CORE.t

module Via_applicative (CORE : Preface_specs.FREEER) :
  Preface_specs.APPLICATIVE with type 'a t = 'a CORE.t

module Via_monad (CORE : Preface_specs.FREEER) :
  Preface_specs.MONAD with type 'a t = 'a CORE.t
