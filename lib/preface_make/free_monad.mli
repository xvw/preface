(** TODO *)

module Over (F : Preface_specs.FUNCTOR) :
  Preface_specs.FREE_MONAD with type 'a f = 'a F.t
