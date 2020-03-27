(** Modules for building [Freer] modules. *)

module Via (F : sig
  type 'a g
end) : Preface_specs.FREER with type 'a g = 'a F.g

module Functor_via_freer (CORE : Preface_specs.FREER) :
  Preface_specs.FUNCTOR with type 'a t = 'a CORE.t

module Applicative_via_freer (CORE : Preface_specs.FREER) :
  Preface_specs.APPLICATIVE with type 'a t = 'a CORE.t

module Monad_via_freer (CORE : Preface_specs.FREER) :
  Preface_specs.MONAD with type 'a t = 'a CORE.t
