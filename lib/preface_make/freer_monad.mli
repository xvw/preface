(** Modules for building [Freer] modules. *)

module Via_type (T : Preface_specs.Freer_monad.TYPE) :
  Preface_specs.FREER_MONAD with type 'a f = 'a T.f
