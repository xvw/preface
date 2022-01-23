module Core_via_map_and_product
    (Req : Preface_specs.Alternative.WITH_MAP_AND_PRODUCT) =
struct
  include Applicative.Core_via_map_and_product (Req)

  let combine = Req.combine
  let neutral = Req.neutral
end

module Core_via_apply (Req : Preface_specs.Alternative.WITH_APPLY) = struct
  include Applicative.Core_via_apply (Req)

  let combine = Req.combine
  let neutral = Req.neutral
end

module Core_via_lift2 (Req : Preface_specs.Alternative.WITH_LIFT2) = struct
  include Applicative.Core_via_lift2 (Req)

  let combine = Req.combine
  let neutral = Req.neutral
end

let reduce' combine neutral list = List.fold_left combine neutral list

module Operation (Core : Preface_specs.Alternative.CORE) = struct
  include Applicative.Operation (Core)
  include Alt.Operation (Core)

  let times n x = Preface_core.Monoid.times Core.combine Core.neutral n x
  let reduce list = reduce' Core.combine Core.neutral list
end

module Syntax (Core : Preface_specs.Alternative.CORE) = Applicative.Syntax (Core)

module Infix
    (Core : Preface_specs.Alternative.CORE)
    (Operation : Preface_specs.Alternative.OPERATION with type 'a t = 'a Core.t) =
struct
  include Applicative.Infix (Core) (Operation)
  include Alt.Infix (Core) (Operation)
end

module Via
    (Core : Preface_specs.Alternative.CORE)
    (Operation : Preface_specs.Alternative.OPERATION)
    (Infix : Preface_specs.Alternative.INFIX)
    (Syntax : Preface_specs.Alternative.SYNTAX) =
struct
  include Core
  include Operation
  include Syntax
  include Infix
  module Infix = Infix
  module Syntax = Syntax
end

module Via_map_and_product
    (Req : Preface_specs.Alternative.WITH_MAP_AND_PRODUCT) =
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

module Via_apply (Req : Preface_specs.Alternative.WITH_APPLY) = struct
  module Core = Core_via_apply (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_lift2 (Req : Preface_specs.Alternative.WITH_LIFT2) = struct
  module Core = Core_via_lift2 (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Over_applicative
    (Applicative : Preface_specs.APPLICATIVE)
    (Req : Preface_specs.Alternative.WITH_NEUTRAL_AND_COMBINE
             with type 'a t = 'a Applicative.t) =
  Via
    (struct
      include Applicative

      let combine = Req.combine
      let neutral = Req.neutral
    end)
    (struct
      include Alt.Operation (struct
        include Applicative
        include Req
      end)

      include Applicative

      let times n x = Preface_core.Monoid.times Req.combine Req.neutral n x
      let reduce list = reduce' Req.combine Req.neutral list
    end)
    (struct
      include Applicative.Infix

      let ( <|> ) = Req.combine
    end)
    (Applicative.Syntax)

module Composition
    (F : Preface_specs.ALTERNATIVE)
    (G : Preface_specs.APPLICATIVE) =
  Over_applicative
    (Applicative.Composition (F) (G))
       (struct
         type 'a t = 'a G.t F.t

         let neutral = F.neutral
         let combine = F.combine
       end)

module From_arrow_plus (A : Preface_specs.ARROW_PLUS) =
  Over_applicative
    (Applicative.From_arrow
       (A))
       (struct
         type 'a t = (unit, 'a) A.t

         let neutral = A.neutral
         let combine x y = A.(x <|> y)
       end)

module Product (F : Preface_specs.ALTERNATIVE) (G : Preface_specs.ALTERNATIVE) =
  Over_applicative
    (Applicative.Product (F) (G))
       (struct
         type 'a t = 'a F.t * 'a G.t

         let neutral = (F.neutral, G.neutral)
         let combine (x1, y1) (x2, y2) = (F.combine x1 x2, G.combine y1 y2)
       end)
