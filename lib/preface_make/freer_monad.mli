(** Modules for building [Freer_monade] modules. *)

module Via_type (T : Preface_specs.Freer_monad.TYPE) :
  Preface_specs.FREER_MONAD with type 'a f = 'a T.t
