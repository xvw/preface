module Core_via_map_and_apply (Req : Preface_specs.Apply.WITH_MAP_AND_APPLY) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_apply.Core_via_map_and_apply (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req : Preface_specs.Apply.WITH_MAP_AND_APPLY with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_apply.CORE with type ('a, _) t := 'a Req.t )
end

module Core_over_functor_via_apply
    (F : Preface_specs.Functor.WITH_MAP)
    (Req : Preface_specs.Apply.WITH_APPLY with type 'a t = 'a F.t) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_apply.Core_over_functor_via_apply
      (struct
        type ('a, 'index) t = 'a Req.t

        include (F : Preface_specs.Functor.WITH_MAP with type 'a t := 'a F.t)
      end)
      (struct
        type ('a, 'index) t = 'a Req.t

        include (Req : Preface_specs.Apply.WITH_APPLY with type 'a t := 'a Req.t)
      end) :
      Preface_specs.Indexed_apply.CORE with type ('a, _) t := 'a Req.t )
end

module Core_via_map_and_product (Req : Preface_specs.Apply.WITH_MAP_AND_PRODUCT) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_apply.Core_via_map_and_product (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req :
          Preface_specs.Apply.WITH_MAP_AND_PRODUCT with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_apply.CORE with type ('a, _) t := 'a Req.t )
end

module Core_over_functor_via_product
    (F : Preface_specs.Functor.WITH_MAP)
    (Req : Preface_specs.Apply.WITH_PRODUCT with type 'a t = 'a F.t) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_apply.Core_over_functor_via_product
      (struct
        type ('a, 'index) t = 'a Req.t

        include (F : Preface_specs.Functor.WITH_MAP with type 'a t := 'a F.t)
      end)
      (struct
        type ('a, 'index) t = 'a Req.t

        include (
          Req : Preface_specs.Apply.WITH_PRODUCT with type 'a t := 'a Req.t )
      end) :
      Preface_specs.Indexed_apply.CORE with type ('a, _) t := 'a Req.t )
end

module Core_via_map_and_lift2 (Req : Preface_specs.Apply.WITH_MAP_AND_LIFT2) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_apply.Core_via_map_and_lift2 (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req : Preface_specs.Apply.WITH_MAP_AND_LIFT2 with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_apply.CORE with type ('a, _) t := 'a Req.t )
end

module Core_over_functor_via_lift2
    (F : Preface_specs.Functor.WITH_MAP)
    (Req : Preface_specs.Apply.WITH_LIFT2 with type 'a t = 'a F.t) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_apply.Core_over_functor_via_lift2
      (struct
        type ('a, 'index) t = 'a Req.t

        include (F : Preface_specs.Functor.WITH_MAP with type 'a t := 'a F.t)
      end)
      (struct
        type ('a, 'index) t = 'a Req.t

        include (Req : Preface_specs.Apply.WITH_LIFT2 with type 'a t := 'a Req.t)
      end) :
      Preface_specs.Indexed_apply.CORE with type ('a, _) t := 'a Req.t )
end

module Operation (Core : Preface_specs.Apply.CORE) = struct
  type 'a t = 'a Core.t

  include (
    Indexed_apply.Operation (struct
      type ('a, 'index) t = 'a Core.t

      include (Core : Preface_specs.Apply.CORE with type 'a t := 'a Core.t)
    end) :
      Preface_specs.Indexed_apply.OPERATION with type ('a, _) t := 'a Core.t )
end

module Syntax (Core : Preface_specs.Apply.CORE) = struct
  type 'a t = 'a Core.t

  include (
    Indexed_apply.Syntax (struct
      type ('a, 'index) t = 'a Core.t

      include (Core : Preface_specs.Apply.CORE with type 'a t := 'a Core.t)
    end) :
      Preface_specs.Indexed_apply.SYNTAX with type ('a, _) t := 'a Core.t )
end

module Infix
    (Core : Preface_specs.Apply.CORE)
    (Operation : Preface_specs.Apply.OPERATION with type 'a t = 'a Core.t) =
struct
  type 'a t = 'a Core.t

  include (
    Indexed_apply.Infix
      (struct
        type ('a, 'index) t = 'a Core.t

        include (Core : Preface_specs.Apply.CORE with type 'a t := 'a Core.t)
      end)
      (struct
        type ('a, 'index) t = 'a Operation.t

        include (
          Operation : Preface_specs.Apply.OPERATION with type 'a t := 'a Core.t )
      end) :
      Preface_specs.Indexed_apply.INFIX with type ('a, _) t := 'a Core.t )
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

module Over_functor_via_apply
    (Functor : Preface_specs.Functor.WITH_MAP)
    (Req : Preface_specs.Apply.WITH_APPLY with type 'a t = 'a Functor.t) =
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
    (Functor : Preface_specs.Functor.WITH_MAP)
    (Req : Preface_specs.Apply.WITH_PRODUCT with type 'a t = 'a Functor.t) =
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
    (Functor : Preface_specs.Functor.WITH_MAP)
    (Req : Preface_specs.Apply.WITH_LIFT2 with type 'a t = 'a Functor.t) =
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

module From_bind (Bind : Preface_specs.Bind.CORE) = struct
  include Via_map_and_product (struct
    type 'a t = 'a Bind.t

    let map = Bind.map
    let product a b = Bind.(bind (fun a -> map (fun b -> (a, b)) b) a)
  end)
end

module From_monad (Monad : Preface_specs.MONAD) = struct
  include From_bind (Monad)
end

module From_applicative (Applicative : Preface_specs.APPLICATIVE) = Applicative
module From_alternative (Alternative : Preface_specs.ALTERNATIVE) = Alternative

module Composition (F : Preface_specs.APPLY) (G : Preface_specs.APPLY) =
  Over_functor_via_apply
    (struct
      type 'a t = 'a G.t F.t

      let map f x = F.map (G.map f) x
    end)
    (struct
      type 'a t = 'a G.t F.t

      let apply f x = F.lift2 G.apply f x
    end)

module From_arrow (A : Preface_specs.ARROW) =
  Over_functor_via_apply
    (struct
      type 'a t = (unit, 'a) A.t

      let map f x = A.(x >>> arrow f)
    end)
    (struct
      type 'a t = (unit, 'a) A.t

      let uncurry f (x, y) = f x y
      let apply f x = A.(f &&& x >>> arrow (uncurry Fun.id))
    end)

module Product (F : Preface_specs.APPLY) (G : Preface_specs.APPLY) =
  Over_functor_via_apply
    (struct
      type 'a t = 'a F.t * 'a G.t

      let map f (x, y) = (F.map f x, G.map f y)
    end)
    (struct
      type 'a t = 'a F.t * 'a G.t

      let apply (f, g) (x, y) = (F.apply f x, G.apply g y)
    end)

module Const (M : Preface_specs.Monoid.CORE) = struct
  type 'a t = Const of M.t

  include (
    Over_functor_via_apply
      (struct
        type nonrec 'a t = 'a t

        let map _f (Const x) = Const x
      end)
      (struct
        type nonrec 'a t = 'a t

        let apply (Const f) (Const x) = Const (M.combine f x)
      end) :
      Preface_specs.APPLY with type 'a t := 'a t )

  let get (Const value) = value
end

module Index (F : Preface_specs.APPLY) = struct
  type ('a, 'index) t = 'a F.t

  include (
    Indexed_apply.Via
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F : Preface_specs.Apply.CORE with type 'a t := 'a F.t)
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F : Preface_specs.Apply.OPERATION with type 'a t := 'a F.t)
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F.Infix : Preface_specs.Apply.INFIX with type 'a t := 'a F.t)
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F.Syntax : Preface_specs.Apply.SYNTAX with type 'a t := 'a F.t)
      end) :
      Preface_specs.INDEXED_APPLY with type ('a, 'index) t := ('a, 'index) t )
end
