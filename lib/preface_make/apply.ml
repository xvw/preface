open Preface_core.Fun

module Core_via_map_and_product (Req : Preface_specs.Apply.WITH_MAP_AND_PRODUCT) =
struct
  include Req

  let apply f a = map (fun (f, a) -> f a) @@ product f a

  let lift2 f x y = apply (map f x) y
end

module Core_via_map_and_apply (Req : Preface_specs.Apply.WITH_MAP_AND_APPLY) =
struct
  include Req

  let product a b = apply (map (fun a b -> (a, b)) a) b

  let lift2 f x y = apply (map f x) y
end

module Core_via_map_and_lift2 (Req : Preface_specs.Apply.WITH_MAP_AND_LIFT2) =
struct
  include Req

  let apply f a = lift2 (fun x -> x) f a

  let product a b = apply (map (fun a b -> (a, b)) a) b
end

module Operation (Core : Preface_specs.Apply.CORE) = struct
  include Functor.Operation (Core)

  let lift = Core.map

  let lift3 f a b = Core.(apply @@ apply (Core.map f a) b)
end

module Syntax (Core : Preface_specs.Apply.CORE) = struct
  type 'a t = 'a Core.t

  let ( let+ ) x f = Core.map f x

  let ( and+ ) = Core.product
end

module Infix
    (Core : Preface_specs.Apply.CORE)
    (Operation : Preface_specs.Apply.OPERATION with type 'a t = 'a Core.t) =
struct
  include Functor.Infix (Core) (Operation)

  let ( <*> ) = Core.apply

  let ( <**> ) a b = Core.lift2 (fun x f -> f x) a b

  let ( *> ) a b = Core.lift2 (fun _x y -> y) a b

  let ( <* ) a b = Core.lift2 const a b
end

module Via
    (Core : Preface_specs.Apply.CORE)
    (Operation : Preface_specs.Apply.OPERATION)
    (Infix : Preface_specs.Apply.INFIX)
    (Syntax : Preface_specs.Apply.SYNTAX) =
struct
  include Core
  include Operation
  include Syntax
  include Infix
  module Infix = Infix
  module Syntax = Syntax
end

module Via_map_and_product (Req : Preface_specs.Apply.WITH_MAP_AND_PRODUCT) =
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

module Via_map_and_apply (Req : Preface_specs.Apply.WITH_MAP_AND_APPLY) = struct
  module Core = Core_via_map_and_apply (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_map_and_lift2 (Req : Preface_specs.Apply.WITH_MAP_AND_LIFT2) = struct
  module Core = Core_via_map_and_lift2 (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module From_applicative (Applicative : Preface_specs.APPLICATIVE) = struct
  include Via_map_and_product (struct
    type 'a t = 'a Applicative.t

    let map = Applicative.map

    let product a b =
      Applicative.apply (Applicative.map (fun a b -> (a, b)) a) b
    ;;
  end)
end

module From_alternative (Alternative : Preface_specs.ALTERNATIVE) = Alternative

module Composition (F : Preface_specs.APPLY) (G : Preface_specs.APPLY) =
Via_map_and_apply (struct
  type 'a t = 'a G.t F.t

  let map f x = F.map (G.map f) x

  let apply f x = F.lift2 G.apply f x
end)

module From_arrow (A : Preface_specs.ARROW) = Via_map_and_apply (struct
  type 'a t = (unit, 'a) A.t

  let map f x = A.(x >>> arrow f)

  let uncurry f (x, y) = f x y

  let apply f x = A.(f &&& x >>> arrow (uncurry Fun.id))
end)
(*
module Product (F : Preface_specs.APPLY) (G : Preface_specs.APPLY) =
Via_map_and_apply (struct
  type 'a t = 'a F.t * 'a G.t

  ('a F.t * 'a G.t -> 'b F.t * 'b G.t) -> ('a F.t * 'a G.t) -> ('b F.t * 'b G.t)
  let map f (x,y) =

  let apply (f, g) (x, y) = (F.apply f x, G.apply g y)
end)

module Const (M : Preface_specs.Monoid.CORE) = struct
  type 'a t = Const of M.t

  include (
    Via_map_and_apply (struct
      type nonrec 'a t = 'a t

      let map f (Const x) = Const (f x)

      let apply (Const f) (Const x) = Const (M.combine f x)
    end) :
      Preface_specs.APPLY with type 'a t := 'a t )

  let get (Const value) = value
end
*)
