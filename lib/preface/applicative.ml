open Fun

module Make_core_via_map_and_product
    (Core : Specs.Applicative.CORE_VIA_MAP_AND_PRODUCT) :
  Specs.Applicative.CORE with type 'a t = 'a Core.t = struct
  include Core

  let apply f a = map (fun (f, a) -> f a) @@ product f a
end

module Make_core_via_apply (Core : Specs.Applicative.CORE_VIA_APPLY) :
  Specs.Applicative.CORE with type 'a t = 'a Core.t = struct
  include Core

  let map f a = apply (pure f) a

  let product a b = apply (apply (pure (fun a b -> a, b)) a) b
end

module Make_operation (Core : Specs.Applicative.CORE) :
  Specs.Applicative.OPERATION with type 'a t = 'a Core.t = struct
  type 'a t = 'a Core.t

  let lift = Core.map

  let lift2 f a =
    let open Core in
    apply @@ apply (pure f) a

  let lift3 f a b =
    let open Core in
    apply @@ apply (apply (pure f) a) b
end

module Make_syntax (Core : Specs.Applicative.CORE) :
  Specs.Applicative.SYNTAX with type 'a t = 'a Core.t = struct
  type 'a t = 'a Core.t

  let ( let+ ) x f = Core.map f x

  let ( and+ ) = Core.product
end

module Make_infix
    (Core : Specs.Applicative.CORE)
    (Operation : Specs.Applicative.OPERATION with type 'a t = 'a Core.t) :
  Specs.Applicative.INFIX with type 'a t = 'a Core.t = struct
  type 'a t = 'a Core.t

  let ( <*> ) = Core.apply

  let ( <**> ) a f = f <*> a

  let ( *> ) a b = Operation.lift2 const b a

  let ( <* ) a b = b *> a
end

module Make
    (Core : Specs.Applicative.CORE)
    (Operation : Specs.Applicative.OPERATION with type 'a t = 'a Core.t)
    (Infix : Specs.Applicative.INFIX with type 'a t = 'a Core.t)
    (Syntax : Specs.Applicative.SYNTAX with type 'a t = 'a Core.t) :
  Specs.APPLICATIVE with type 'a t = 'a Core.t = struct
  include Core
  include Operation
  include Syntax
  include Infix
  module Infix = Infix
  module Syntax = Syntax
end

module Make_via_map_and_product
    (Core_via_map_and_product : Specs.Applicative.CORE_VIA_MAP_AND_PRODUCT) :
  Specs.APPLICATIVE with type 'a t = 'a Core_via_map_and_product.t = struct
  module Core = Make_core_via_map_and_product (Core_via_map_and_product)
  module Operation = Make_operation (Core)
  module Syntax = Make_syntax (Core)
  module Infix = Make_infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Make_via_apply (Core_via_apply : Specs.Applicative.CORE_VIA_APPLY) :
  Specs.APPLICATIVE with type 'a t = 'a Core_via_apply.t = struct
  module Core = Make_core_via_apply (Core_via_apply)
  module Operation = Make_operation (Core)
  module Syntax = Make_syntax (Core)
  module Infix = Make_infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Make_via_monad (Monad : Specs.MONAD) :
  Specs.APPLICATIVE with type 'a t = 'a Monad.t = struct
  include Make_via_apply (struct
    type 'a t = 'a Monad.t

    let pure = Monad.return

    let apply fs xs =
      let open Monad.Syntax in
      let* f = fs in
      let* x = xs in
      pure (f x)
  end)
end
