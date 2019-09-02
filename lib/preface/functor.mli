(** Incarnation of a [Functor].  *)

module Via_map (REQUIREMENT : Specs.Functor.REQUIREMENT) :
  Specs.FUNCTOR with type 'a t = 'a REQUIREMENT.t
(** Incarnation of a [Functor] for an ['a t] with standard
    Requirements (map). 
*)

module Via_map_with_replace (REQUIREMENT : Specs.Functor.FULL_REQUIREMENT) :
  Specs.FUNCTOR with type 'a t = 'a REQUIREMENT.t
(** Incarnation of a [Functor] for an ['a t] with full
    Requirements (map and replace). 
*)
