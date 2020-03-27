(** Modules for building [Free] modules. *)

module Via_functor (Functor : Preface_specs.FUNCTOR) :
  Preface_specs.FREE with module Functor = Functor

module Functor_via_free (Free : Preface_specs.FREE) :
  Preface_specs.FUNCTOR with type 'a t = 'a Free.t

module Applicative_via_free (Free : Preface_specs.FREE) :
  Preface_specs.APPLICATIVE with type 'a t = 'a Free.t

module Monad_via_free (Free : Preface_specs.FREE) :
  Preface_specs.MONAD with type 'a t = 'a Free.t
