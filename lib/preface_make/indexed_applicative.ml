open Preface_core.Fun

module Core_via_pure_map_and_product
    (Req : Preface_specs.Indexed_applicative.WITH_PURE_MAP_AND_PRODUCT) =
struct
  include Req

  let apply f a = map (fun (f, a) -> f a) @@ product f a
  let lift2 f x y = apply (map f x) y
end

module Core_via_pure_and_apply
    (Req : Preface_specs.Indexed_applicative.WITH_PURE_AND_APPLY) =
struct
  include Req

  let map f a = apply (pure f) a
  let product a b = apply (apply (pure (fun a b -> (a, b))) a) b
  let lift2 f x y = apply (map f x) y
end

module Core_via_pure_and_lift2
    (Req : Preface_specs.Indexed_applicative.WITH_PURE_AND_LIFT2) =
struct
  include Req

  let apply f a = lift2 (fun x -> x) f a
  let map f a = apply (pure f) a
  let product a b = apply (apply (pure (fun a b -> (a, b))) a) b
end

module Operation (Core : Preface_specs.Indexed_applicative.CORE) = struct
  include Indexed_apply.Operation (Core)

  let lift = Core.map
  let lift3 f a b = Core.(apply @@ apply (apply (pure f) a) b)
end

module Syntax (Core : Preface_specs.Indexed_applicative.CORE) = struct
  type ('a, 'index) t = ('a, 'index) Core.t

  let ( let+ ) x f = Core.map f x
  let ( and+ ) = Core.product
end

module Infix
    (Core : Preface_specs.Indexed_applicative.CORE)
    (Operation : Preface_specs.Indexed_applicative.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t) =
struct
  include Indexed_apply.Infix (Core) (Operation)

  let ( <*> ) = Core.apply
  let ( <**> ) a b = Core.lift2 (fun x f -> f x) a b
  let ( *> ) a b = Core.lift2 (fun _x y -> y) a b
  let ( <* ) a b = Core.lift2 const a b
end

module Via
    (Core : Preface_specs.Indexed_applicative.CORE)
    (Operation : Preface_specs.Indexed_applicative.OPERATION)
    (Infix : Preface_specs.Indexed_applicative.INFIX)
    (Syntax : Preface_specs.Indexed_applicative.SYNTAX) =
struct
  include Core
  include Operation
  include Syntax
  include Infix
  module Infix = Infix
  module Syntax = Syntax
end

module Via_pure_map_and_product
    (Req : Preface_specs.Indexed_applicative.WITH_PURE_MAP_AND_PRODUCT) =
struct
  module Core = Core_via_pure_map_and_product (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_pure_and_apply
    (Req : Preface_specs.Indexed_applicative.WITH_PURE_AND_APPLY) =
struct
  module Core = Core_via_pure_and_apply (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_pure_and_lift2
    (Req : Preface_specs.Indexed_applicative.WITH_PURE_AND_LIFT2) =
struct
  module Core = Core_via_pure_and_lift2 (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Over_apply
    (Apply : Preface_specs.INDEXED_APPLY)
    (Req : Preface_specs.Indexed_applicative.WITH_PURE
             with type ('a, 'index) t = ('a, 'index) Apply.t) =
struct
  include Via_pure_and_apply (struct
    type ('a, 'index) t = ('a, 'index) Apply.t

    let pure = Req.pure
    let apply = Apply.apply
  end)
end
