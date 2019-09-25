open Fun

module Make_core_via_bind (Core : Specs.Monad.CORE_VIA_BIND) :
  Specs.Monad.CORE with type 'a t = 'a Core.t = struct
  include Core

  let join m = bind id m

  let map f m = bind (return <% f) m

  let compose_left_to_right f g x = bind g (f x)
end

module Make_core_via_map_and_join (Core : Specs.Monad.CORE_VIA_MAP_AND_JOIN) :
  Specs.Monad.CORE with type 'a t = 'a Core.t = struct
  include Core

  let bind f m = join (map f m)

  let compose_left_to_right f g x = bind g (f x)
end

module Make_core_via_kleisli_composition
    (Core : Specs.Monad.CORE_VIA_KLEISLI_COMPOSITION) :
  Specs.Monad.CORE with type 'a t = 'a Core.t = struct
  include Core

  let bind f m = (compose_left_to_right (constant m) f) ()

  let join m = bind id m

  let map f m = bind (return <% f) m
end
