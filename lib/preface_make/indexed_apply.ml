open Preface_core.Fun

module Core_via_map_and_apply
    (Req : Preface_specs.Indexed_apply.WITH_MAP_AND_APPLY) =
struct
  include Req

  let product a b = apply (map (fun a b -> (a, b)) a) b
  let lift2 f x y = apply (map f x) y
end

module Core_over_functor_via_apply
    (Functor : Preface_specs.Indexed_functor.WITH_MAP)
    (Req : Preface_specs.Indexed_apply.WITH_APPLY
             with type ('a, 'index) t = ('a, 'index) Functor.t) =
struct
  include Core_via_map_and_apply (struct
    include Functor
    include Req
  end)
end

module Core_via_map_and_product
    (Req : Preface_specs.Indexed_apply.WITH_MAP_AND_PRODUCT) =
struct
  include Req

  let apply f a = map (fun (f, a) -> f a) @@ product f a
  let lift2 f x y = apply (map f x) y
end

module Core_over_functor_via_product
    (Functor : Preface_specs.Indexed_functor.WITH_MAP)
    (Req : Preface_specs.Indexed_apply.WITH_PRODUCT
             with type ('a, 'index) t = ('a, 'index) Functor.t) =
struct
  include Core_via_map_and_product (struct
    include Functor
    include Req
  end)
end

module Core_via_map_and_lift2
    (Req : Preface_specs.Indexed_apply.WITH_MAP_AND_LIFT2) =
struct
  include Req

  let apply f a = lift2 (fun x -> x) f a
  let product a b = apply (map (fun a b -> (a, b)) a) b
end

module Core_over_functor_via_lift2
    (Functor : Preface_specs.Indexed_functor.WITH_MAP)
    (Req : Preface_specs.Indexed_apply.WITH_LIFT2
             with type ('a, 'index) t = ('a, 'index) Functor.t) =
struct
  include Core_via_map_and_lift2 (struct
    include Functor
    include Req
  end)
end

module Operation (Core : Preface_specs.Indexed_apply.CORE) = struct
  include Indexed_functor.Operation (Core)

  let lift = Core.map
  let lift3 f a b = Core.(apply @@ apply (Core.map f a) b)
end

module Syntax (Core : Preface_specs.Indexed_apply.CORE) = struct
  type ('a, 'index) t = ('a, 'index) Core.t

  let ( let+ ) x f = Core.map f x
  let ( and+ ) = Core.product
end

module Infix
    (Core : Preface_specs.Indexed_apply.CORE)
    (Operation : Preface_specs.Indexed_apply.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t) =
struct
  include Indexed_functor.Infix (Core) (Operation)

  let ( <*> ) = Core.apply
  let ( <**> ) a b = Core.lift2 (fun x f -> f x) a b
  let ( *> ) a b = Core.lift2 (flip const) a b
  let ( <* ) a b = Core.lift2 const a b
end

module Via
    (Core : Preface_specs.Indexed_apply.CORE)
    (Operation : Preface_specs.Indexed_apply.OPERATION)
    (Infix : Preface_specs.Indexed_apply.INFIX)
    (Syntax : Preface_specs.Indexed_apply.SYNTAX) =
struct
  include Core
  include Operation
  include Syntax
  include Infix
  module Infix = Infix
  module Syntax = Syntax
end

module Via_map_and_apply (Req : Preface_specs.Indexed_apply.WITH_MAP_AND_APPLY) =
struct
  module Core = Core_via_map_and_apply (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_map_and_product
    (Req : Preface_specs.Indexed_apply.WITH_MAP_AND_PRODUCT) =
struct
  module Core = Core_via_map_and_product (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_map_and_lift2 (Req : Preface_specs.Indexed_apply.WITH_MAP_AND_LIFT2) =
struct
  module Core = Core_via_map_and_lift2 (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Over_functor_via_apply
    (Functor : Preface_specs.Indexed_functor.WITH_MAP)
    (Req : Preface_specs.Indexed_apply.WITH_APPLY
             with type ('a, 'index) t = ('a, 'index) Functor.t) =
struct
  module Core = Core_over_functor_via_apply (Functor) (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Over_functor_via_product
    (Functor : Preface_specs.Indexed_functor.WITH_MAP)
    (Req : Preface_specs.Indexed_apply.WITH_PRODUCT
             with type ('a, 'index) t = ('a, 'index) Functor.t) =
struct
  module Core = Core_over_functor_via_product (Functor) (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Over_functor_via_lift2
    (Functor : Preface_specs.Indexed_functor.WITH_MAP)
    (Req : Preface_specs.Indexed_apply.WITH_LIFT2
             with type ('a, 'index) t = ('a, 'index) Functor.t) =
struct
  module Core = Core_over_functor_via_lift2 (Functor) (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end
