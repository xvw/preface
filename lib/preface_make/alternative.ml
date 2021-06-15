module Core_via_map_and_product
    (Req : Preface_specs.Alternative.WITH_MAP_AND_PRODUCT) :
  Preface_specs.Alternative.CORE with type 'a t = 'a Req.t = struct
  include Applicative.Core_via_map_and_product (Req)

  let combine = Req.combine

  let neutral = Req.neutral
end

module Core_via_apply (Req : Preface_specs.Alternative.WITH_APPLY) :
  Preface_specs.Alternative.CORE with type 'a t = 'a Req.t = struct
  include Applicative.Core_via_apply (Req)

  let combine = Req.combine

  let neutral = Req.neutral
end

module Core_via_lift2 (Req : Preface_specs.Alternative.WITH_LIFT2) :
  Preface_specs.Alternative.CORE with type 'a t = 'a Req.t = struct
  include Applicative.Core_via_lift2 (Req)

  let combine = Req.combine

  let neutral = Req.neutral
end

let reduce' combine neutral list = List.fold_left combine neutral list

module Operation (Core : Preface_specs.Alternative.CORE) :
  Preface_specs.Alternative.OPERATION with type 'a t = 'a Core.t = struct
  include Applicative.Operation (Core)
  include Alt.Operation (Core)

  let reduce list = reduce' Core.combine Core.neutral list
end

module Syntax (Core : Preface_specs.Alternative.CORE) :
  Preface_specs.Alternative.SYNTAX with type 'a t = 'a Core.t =
  Applicative.Syntax (Core)

module Infix
    (Core : Preface_specs.Alternative.CORE)
    (Operation : Preface_specs.Alternative.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Alternative.INFIX with type 'a t = 'a Core.t = struct
  include Applicative.Infix (Core) (Operation)
  include Alt.Infix (Core) (Operation)
end

module Via
    (Core : Preface_specs.Alternative.CORE)
    (Operation : Preface_specs.Alternative.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Alternative.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Alternative.SYNTAX with type 'a t = 'a Core.t) :
  Preface_specs.ALTERNATIVE with type 'a t = 'a Core.t = struct
  include Core
  include Operation
  include Syntax
  include Infix
  module Infix = Infix
  module Syntax = Syntax
end

module Via_map_and_product
    (Req : Preface_specs.Alternative.WITH_MAP_AND_PRODUCT) :
  Preface_specs.ALTERNATIVE with type 'a t = 'a Req.t = struct
  module Core = Core_via_map_and_product (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_apply (Req : Preface_specs.Alternative.WITH_APPLY) :
  Preface_specs.ALTERNATIVE with type 'a t = 'a Req.t = struct
  module Core = Core_via_apply (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_lift2 (Req : Preface_specs.Alternative.WITH_LIFT2) :
  Preface_specs.ALTERNATIVE with type 'a t = 'a Req.t = struct
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
             with type 'a t = 'a Applicative.t) :
  Preface_specs.ALTERNATIVE with type 'a t = 'a Req.t =
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

      let reduce list = reduce' Req.combine Req.neutral list
    end)
    (struct
      type 'a t = 'a Applicative.t

      include Applicative.Infix

      let ( <|> ) = Req.combine
    end)
    (struct
      type 'a t = 'a Applicative.t

      include Applicative.Syntax
    end)

module Composition
    (F : Preface_specs.ALTERNATIVE)
    (G : Preface_specs.APPLICATIVE) :
  Preface_specs.ALTERNATIVE with type 'a t = 'a G.t F.t =
  Over_applicative
    (Applicative.Composition (F) (G))
       (struct
         type 'a t = 'a G.t F.t

         let neutral = F.neutral

         let combine = F.combine
       end)

module From_arrow_plus (A : Preface_specs.ARROW_PLUS) :
  Preface_specs.ALTERNATIVE with type 'a t = (unit, 'a) A.t =
  Over_applicative
    (Applicative.From_arrow
       (A))
       (struct
         type 'a t = (unit, 'a) A.t

         let neutral = A.neutral

         let combine x y = A.(x <|> y)
       end)

module Product (F : Preface_specs.ALTERNATIVE) (G : Preface_specs.ALTERNATIVE) :
  Preface_specs.ALTERNATIVE with type 'a t = 'a F.t * 'a G.t =
  Over_applicative
    (Applicative.Product (F) (G))
       (struct
         type 'a t = 'a F.t * 'a G.t

         let neutral = (F.neutral, G.neutral)

         let combine (x1, y1) (x2, y2) = (F.combine x1 x2, G.combine y1 y2)
       end)
