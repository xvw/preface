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

  let lift2 f a = Core.apply @@ Core.apply (Core.pure f) a

  let lift3 f a b = Core.apply @@ Core.apply (Core.apply (Core.pure f) a) b
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

module Make_via_map_and_product
    (Core_via_map_and_product : Specs.Applicative.CORE_VIA_MAP_AND_PRODUCT) :
  Specs.APPLICATIVE with type 'a t = 'a Core_via_map_and_product.t = struct
  module Core = Make_core_via_map_and_product (Core_via_map_and_product)
  include Core
  module Operation = Make_operation (Core)
  include Operation
  module Syntax = Make_syntax (Core)
  include Syntax
  module Infix = Make_infix (Core) (Operation)
  include Infix
end

module Make_via_apply (Core_via_apply : Specs.Applicative.CORE_VIA_APPLY) :
  Specs.APPLICATIVE with type 'a t = 'a Core_via_apply.t = struct
  module Core = Make_core_via_apply (Core_via_apply)
  include Core
  module Operation = Make_operation (Core)
  include Operation
  module Syntax = Make_syntax (Core)
  include Syntax
  module Infix = Make_infix (Core) (Operation)
  include Infix
end
