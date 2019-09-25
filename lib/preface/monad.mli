module Make_core_via_bind (Core : Specs.Monad.CORE_VIA_BIND) :
  Specs.Monad.CORE with type 'a t = 'a Core.t

module Make_core_via_map_and_join (Core : Specs.Monad.CORE_VIA_MAP_AND_JOIN) :
  Specs.Monad.CORE with type 'a t = 'a Core.t

module Make_core_via_kleisli_composition
    (Core : Specs.Monad.CORE_VIA_KLEISLI_COMPOSITION) :
  Specs.Monad.CORE with type 'a t = 'a Core.t
