(** Incarnation of a [Functor].  *)

module For (REQUIREMENT : Specs.Functor.REQUIREMENT) :
  Specs.FUNCTOR with type 'a t = 'a REQUIREMENT.t
(** Incarnation of a [Functor] for an ['a t] with standard
    Requirements. 
*)
