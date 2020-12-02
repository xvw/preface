open Preface_core.Fun

module Core_via_bind (Core : Preface_specs.Monad.CORE_WITH_BIND) :
  Preface_specs.Monad.CORE with type 'a t = 'a Core.t = struct
  include Core

  let join m = bind id m

  let map f m = bind (return <% f) m

  let compose_left_to_right f g x = bind g (f x)
end

module Core_via_map_and_join (Core : Preface_specs.Monad.CORE_WITH_MAP_AND_JOIN) :
  Preface_specs.Monad.CORE with type 'a t = 'a Core.t = struct
  include Core

  let bind f m = join (map f m)

  let compose_left_to_right f g x = bind g (f x)
end

module Core_via_kleisli_composition
    (Core : Preface_specs.Monad.CORE_WITH_KLEISLI_COMPOSITION) :
  Preface_specs.Monad.CORE with type 'a t = 'a Core.t = struct
  include Core

  let bind f m = (compose_left_to_right (constant m) f) ()

  let join m = bind id m

  let map f m = bind (return <% f) m
end

module Operation (Core : Preface_specs.Monad.CORE) :
  Preface_specs.Monad.OPERATION with type 'a t = 'a Core.t = struct
  type 'a t = 'a Core.t

  let void _ = Core.return ()

  let compose_right_to_left f g x = Core.compose_left_to_right g f x
end

module Lift (Core : Preface_specs.Monad.CORE) :
  Preface_specs.Monad.LIFT with type 'a t = 'a Core.t = struct
  type 'a t = 'a Core.t

  let lift = Core.map

  let lift2 f ma mb = Core.(bind (fun a -> bind (fun b -> return (f a b)) mb) ma)

  let lift3 f ma mb mc =
    let open Core in
    bind (fun a -> bind (fun b -> bind (fun c -> return (f a b c)) mc) mb) ma
  ;;
end

module Syntax (Core : Preface_specs.Monad.CORE) :
  Preface_specs.Monad.SYNTAX with type 'a t = 'a Core.t = struct
  type 'a t = 'a Core.t

  let ( let* ) m f = Core.bind f m

  let ( let+ ) m f = Core.map f m
end

module Infix
    (Core : Preface_specs.Monad.CORE)
    (Operation : Preface_specs.Monad.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Monad.INFIX with type 'a t = 'a Core.t = struct
  type 'a t = 'a Core.t

  let ( >|= ) x f = Core.map f x

  let ( =|< ) = Core.map

  let ( >>= ) x f = Core.bind f x

  let ( =<< ) = Core.bind

  let ( >=> ) = Core.compose_left_to_right

  let ( <=< ) = Operation.compose_right_to_left

  let ( >> ) ma mb = ma >>= (fun _ -> mb)

  let ( << ) ma _ = ma
end

module Via
    (Core : Preface_specs.Monad.CORE)
    (Operation : Preface_specs.Monad.OPERATION with type 'a t = 'a Core.t)
    (Lift : Preface_specs.Monad.LIFT with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Monad.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Monad.SYNTAX with type 'a t = 'a Core.t) :
  Preface_specs.MONAD with type 'a t = 'a Core.t = struct
  include Core
  include Operation
  include Lift
  include Syntax
  include Infix
  module Syntax = Syntax
  module Infix = Infix
end

module Via_bind (Core_with_bind : Preface_specs.Monad.CORE_WITH_BIND) :
  Preface_specs.MONAD with type 'a t = 'a Core_with_bind.t = struct
  module Core = Core_via_bind (Core_with_bind)
  module Operation = Operation (Core)
  module Lift = Lift (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Lift
  include Syntax
  include Infix
end

module Via_map_and_join
    (Core_with_map_and_join : Preface_specs.Monad.CORE_WITH_MAP_AND_JOIN) :
  Preface_specs.MONAD with type 'a t = 'a Core_with_map_and_join.t = struct
  module Core = Core_via_map_and_join (Core_with_map_and_join)
  module Operation = Operation (Core)
  module Lift = Lift (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Lift
  include Syntax
  include Infix
end

module Via_kleisli_composition
    (Core_with_kleisli_composition : Preface_specs.Monad
                                     .CORE_WITH_KLEISLI_COMPOSITION) :
  Preface_specs.MONAD with type 'a t = 'a Core_with_kleisli_composition.t =
struct
  module Core = Core_via_kleisli_composition (Core_with_kleisli_composition)
  module Operation = Operation (Core)
  module Lift = Lift (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Lift
  include Syntax
  include Infix
end

module From_monad_plus (Monad_plus : Preface_specs.MONAD_PLUS) :
  Preface_specs.MONAD with type 'a t = 'a Monad_plus.t =
  Monad_plus
