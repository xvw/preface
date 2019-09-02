(** Incarnation of a [Functor].  *)

module Make (REQUIREMENT : Specs.Functor.REQUIREMENT) :
  Specs.FUNCTOR with type 'a t = 'a REQUIREMENT.t
(** Incarnation of a [Functor] for an ['a t] with standard
    Requirements ([map]). 
*)

module Make_with_replace (REQUIREMENT : Specs.Functor.REQUIREMENT_WITH_REPLACE) :
  Specs.FUNCTOR with type 'a t = 'a REQUIREMENT.t
(** Incarnation of a [Functor] for an ['a t] with full
    Requirements ([map] and [replace]). 
*)
