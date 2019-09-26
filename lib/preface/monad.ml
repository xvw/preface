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

module Make_operation (Core : Specs.Monad.CORE) :
  Specs.Monad.OPERATION with type 'a t = 'a Core.t = struct
  type 'a t = 'a Core.t

  let void _ = Core.return ()

  let compose_right_to_left f g x = Core.compose_left_to_right g f x

  let lift = Core.map

  let lift2 f ma mb =
    let open Core in
    bind (fun a -> bind (fun b -> return (f a b)) mb) ma

  let lift3 f ma mb mc =
    let open Core in
    bind (fun a -> bind (fun b -> bind (fun c -> return (f a b c)) mc) mb) ma
end

module Make_syntax (Core : Specs.Monad.CORE) :
  Specs.Monad.SYNTAX with type 'a t = 'a Core.t = struct
  type 'a t = 'a Core.t

  let ( let* ) m f = Core.bind f m
end

module Make_infix
    (Core : Specs.Monad.CORE)
    (Operation : Specs.Monad.OPERATION with type 'a t = 'a Core.t) :
  Specs.Monad.INFIX with type 'a t = 'a Core.t = struct
  type 'a t = 'a Core.t

  let ( >|= ) x f = Core.map f x

  let ( >>= ) x f = Core.bind f x

  let ( =<< ) = Core.bind

  let ( >=> ) = Core.compose_left_to_right

  let ( <=< ) = Operation.compose_right_to_left

  let ( >> ) ma mb = ma >>= fun _ -> mb

  let ( << ) ma _ = ma
end

module Make
    (Core : Specs.Monad.CORE)
    (Operation : Specs.Monad.OPERATION with type 'a t = 'a Core.t)
    (Infix : Specs.Monad.INFIX with type 'a t = 'a Core.t)
    (Syntax : Specs.Monad.SYNTAX with type 'a t = 'a Core.t) :
  Specs.MONAD with type 'a t = 'a Core.t = struct
  include Core
  include Operation
  include Syntax
  include Infix
  module Syntax = Syntax
  module Infix = Infix
end

module Make_via_bind (Core_via_bind : Specs.Monad.CORE_VIA_BIND) :
  Specs.MONAD with type 'a t = 'a Core_via_bind.t = struct
  module Core = Make_core_via_bind (Core_via_bind)
  module Operation = Make_operation (Core)
  module Syntax = Make_syntax (Core)
  module Infix = Make_infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Make_via_map_and_join
    (Core_via_map_and_join : Specs.Monad.CORE_VIA_MAP_AND_JOIN) :
  Specs.MONAD with type 'a t = 'a Core_via_map_and_join.t = struct
  module Core = Make_core_via_map_and_join (Core_via_map_and_join)
  module Operation = Make_operation (Core)
  module Syntax = Make_syntax (Core)
  module Infix = Make_infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Make_via_kleisli_composition
    (Core_via_kleisli_composition : Specs.Monad.CORE_VIA_KLEISLI_COMPOSITION) :
  Specs.MONAD with type 'a t = 'a Core_via_kleisli_composition.t = struct
  module Core = Make_core_via_kleisli_composition (Core_via_kleisli_composition)
  module Operation = Make_operation (Core)
  module Syntax = Make_syntax (Core)
  module Infix = Make_infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end
