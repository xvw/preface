(** Modules for building [Free] modules. *)

module Free (Functor : Preface_specs.Functor.CORE) :
  Preface_specs.FREE with module F = Functor

module Free_functor (Free : Preface_specs.FREE) :
  Preface_specs.FUNCTOR with type 'a t = 'a Free.t

module Free_applicative (Free : Preface_specs.FREE) :
  Preface_specs.APPLICATIVE with type 'a t = 'a Free.t

module Free_monad (Free : Preface_specs.FREE) :
  Preface_specs.MONAD with type 'a t = 'a Free.t
