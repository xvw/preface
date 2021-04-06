module Core_over_category_and_via_arrow_and_fst
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_zero.WITH_ARROW_AND_FST
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_zero.CORE with type ('a, 'b) t = ('a, 'b) Req.t = struct
  include Arrow.Core_over_category_and_via_arrow_and_fst (Category) (Req)

  let neutral = Req.neutral
end

module Core_over_category_and_via_arrow_and_split
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_zero.WITH_ARROW_AND_SPLIT
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_zero.CORE with type ('a, 'b) t = ('a, 'b) Req.t = struct
  include Arrow.Core_over_category_and_via_arrow_and_split (Category) (Req)

  let neutral = Req.neutral
end

module Operation_over_category = Arrow.Operation_over_category
module Alias = Arrow.Alias
module Infix_over_category = Arrow.Infix_over_category

module Via
    (Core : Preface_specs.Arrow_zero.CORE)
    (Operation : Preface_specs.Arrow_zero.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t)
    (Alias : Preface_specs.Arrow_zero.ALIAS
               with type ('a, 'b) t = ('a, 'b) Operation.t)
    (Infix : Preface_specs.Arrow_zero.INFIX
               with type ('a, 'b) t = ('a, 'b) Alias.t) :
  Preface_specs.ARROW_ZERO with type ('a, 'b) t = ('a, 'b) Infix.t = struct
  include Core
  include Operation
  include Alias
  include Infix
  module Infix = Infix
end

module Over_category_and_via_arrow_and_fst
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_zero.WITH_ARROW_AND_FST
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_ZERO with type ('a, 'b) t = ('a, 'b) Req.t = struct
  module Core = Core_over_category_and_via_arrow_and_fst (Category) (Req)
  module Operation = Operation_over_category (Category) (Core)
  module Alias = Alias (Operation)
  module Infix = Infix_over_category (Category) (Core) (Operation)
  include Core
  include Operation
  include Alias
  include Infix
end

module Over_category_and_via_arrow_an_split
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_zero.WITH_ARROW_AND_SPLIT
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_ZERO with type ('a, 'b) t = ('a, 'b) Req.t = struct
  module Core = Core_over_category_and_via_arrow_and_split (Category) (Req)
  module Operation = Operation_over_category (Category) (Core)
  module Alias = Alias (Operation)
  module Infix = Infix_over_category (Category) (Core) (Operation)
  include Core
  include Operation
  include Alias
  include Infix
end

module Over_arrow
    (Arrow : Preface_specs.ARROW)
    (Req : Preface_specs.Arrow_zero.WITH_NEUTRAL
             with type ('a, 'b) t = ('a, 'b) Arrow.t) :
  Preface_specs.ARROW_ZERO with type ('a, 'b) t = ('a, 'b) Req.t = struct
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

module From_monad_plus (Monad : Preface_specs.Monad_plus.CORE) :
  Preface_specs.ARROW_ZERO with type ('a, 'b) t = 'a -> 'b Monad.t =
  Over_arrow
    (Arrow.From_monad
       (Monad))
       (struct
         type ('a, 'b) t = 'a -> 'b Monad.t

         let neutral _ = Monad.neutral
       end)

module From_arrow_plus (Plus : Preface_specs.ARROW_PLUS) :
  Preface_specs.ARROW_ZERO with type ('a, 'b) t = ('a, 'b) Plus.t =
  Plus

module Product (F : Preface_specs.ARROW_ZERO) (G : Preface_specs.ARROW_ZERO) :
  Preface_specs.ARROW_ZERO with type ('a, 'b) t = ('a, 'b) F.t * ('a, 'b) G.t =
  Over_arrow
    (Arrow.Product (F) (G))
       (struct
         type ('a, 'b) t = ('a, 'b) F.t * ('a, 'b) G.t

         let neutral = (F.neutral, G.neutral)
       end)
