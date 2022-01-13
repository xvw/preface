module Core_over_category_and_via_arrow_and_fst
    (Category : Preface_specs.Category.CORE)
    (Req : Preface_specs.Arrow_plus.WITH_ARROW_AND_FST
             with type ('a, 'b) t = ('a, 'b) Category.t) =
struct
  include Arrow.Core_over_category_and_via_arrow_and_fst (Category) (Req)

  let combine = Req.combine

  let neutral = Req.neutral
end

module Core_over_category_and_via_arrow_and_split
    (Category : Preface_specs.Category.CORE)
    (Req : Preface_specs.Arrow_plus.WITH_ARROW_AND_SPLIT
             with type ('a, 'b) t = ('a, 'b) Category.t) =
struct
  include Arrow.Core_over_category_and_via_arrow_and_split (Category) (Req)

  let combine = Req.combine

  let neutral = Req.neutral
end

module Operation_over_category
    (Category : Preface_specs.Category.OPERATION)
    (Core : Preface_specs.Arrow_plus.CORE
              with type ('a, 'b) t = ('a, 'b) Category.t) =
struct
  include Arrow.Operation_over_category (Category) (Core)

  let times_nel n x = Preface_core.Monoid.times_nel Core.combine n x

  let reduce_nel list = Preface_core.Monoid.reduce_nel Core.combine list

  let times n x = Preface_core.Monoid.times Core.combine Core.neutral n x

  let reduce list = Preface_core.Monoid.reduce Core.combine Core.neutral list
end

module Alias = Arrow.Alias

module Infix_over_category
    (Category : Preface_specs.Category.INFIX)
    (Core : Preface_specs.Arrow_plus.CORE
              with type ('a, 'b) t = ('a, 'b) Category.t)
    (Operation : Preface_specs.Arrow_plus.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) =
struct
  include Arrow.Infix_over_category (Category) (Core) (Operation)

  let ( <|> ) = Core.combine
end

module Via
    (Core : Preface_specs.Arrow_plus.CORE)
    (Operation : Preface_specs.Arrow_plus.OPERATION)
    (Alias : Preface_specs.Arrow_plus.ALIAS)
    (Infix : Preface_specs.Arrow_plus.INFIX) =
struct
  include Core
  include Operation
  include Alias
  include Infix
  module Infix = Infix
end

module Over_category_and_via_arrow_and_fst
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_plus.WITH_ARROW_AND_FST
             with type ('a, 'b) t = ('a, 'b) Category.t) =
struct
  module Core = Core_over_category_and_via_arrow_and_fst (Category) (Req)
  module Operation = Operation_over_category (Category) (Core)
  module Alias = Alias (Operation)
  module Infix = Infix_over_category (Category.Infix) (Core) (Operation)
  include Core
  include Operation
  include Alias
  include Infix
end

module Over_category_and_via_arrow_and_split
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_plus.WITH_ARROW_AND_SPLIT
             with type ('a, 'b) t = ('a, 'b) Category.t) =
struct
  module Core = Core_over_category_and_via_arrow_and_split (Category) (Req)
  module Operation = Operation_over_category (Category) (Core)
  module Alias = Alias (Operation)
  module Infix = Infix_over_category (Category.Infix) (Core) (Operation)
  include Core
  include Operation
  include Alias
  include Infix
end

module Over_arrow
    (Arrow : Preface_specs.ARROW)
    (Req : Preface_specs.Arrow_plus.WITH_COMBINE_AND_NEUTRAL
             with type ('a, 'b) t = ('a, 'b) Arrow.t) =
struct
  module Core_aux =
    Core_over_category_and_via_arrow_and_fst
      (Arrow)
      (struct
        include Arrow
        include Req
      end)

  module Operation_aux = Operation_over_category (Arrow) (Core_aux)
  module Infix_aux = Infix_over_category (Arrow) (Core_aux) (Operation_aux)
  include Core_aux
  include Operation_aux
  include Arrow

  module Infix = struct
    include Arrow.Infix
    include Infix_aux
  end

  include Infix
end

module From_monad_plus (Monad : Preface_specs.Monad_plus.CORE) =
  Over_arrow
    (Arrow.From_monad
       (Monad))
       (struct
         type ('a, 'b) t = 'a -> 'b Monad.t

         let combine f g x = Monad.combine (f x) (g x)

         let neutral _ = Monad.neutral
       end)

module Product (F : Preface_specs.ARROW_PLUS) (G : Preface_specs.ARROW_PLUS) =
  Over_arrow
    (Arrow.Product (F) (G))
       (struct
         type ('a, 'b) t = ('a, 'b) F.t * ('a, 'b) G.t

         let neutral = (F.neutral, G.neutral)

         let combine (x1, y1) (x2, y2) = (F.combine x1 x2, G.combine y1 y2)
       end)
