open Preface_core.Shims

let extract = function Either.Left x | Either.Right x -> x

module Choose_over_left
    (Category : Preface_specs.CATEGORY)
    (Arrow : Preface_specs.Arrow.CORE with type ('a, 'b) t = ('a, 'b) Category.t)
    (Left : Preface_specs.Arrow_choice.WITH_LEFT
              with type ('a, 'b) t = ('a, 'b) Arrow.t) =
struct
  let choose f g =
    let open Category in
    Left.left f
    >>> Arrow.arrow Either.swap
    >>> Left.left g
    >>> Arrow.arrow Either.swap
  ;;
end

module Left_over_choose
    (Category : Preface_specs.CATEGORY)
    (Choose : Preface_specs.Arrow_choice.WITH_CHOOSE
                with type ('a, 'b) t = ('a, 'b) Category.t) =
struct
  let left x = Choose.choose x Category.id
end

module Core_over_category_and_via_arrow_and_fst_and_left
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_choice.WITH_ARROW_AND_FST_AND_LEFT
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_choice.CORE with type ('a, 'b) t = ('a, 'b) Req.t = struct
  module C = Arrow.Core_over_category_and_via_arrow_and_fst (Category) (Req)
  include C

  let left = Req.left

  include Choose_over_left (Category) (C) (Req)
end

module Core_over_category_and_via_arrow_and_split_and_left
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_choice.WITH_ARROW_AND_SPLIT_AND_LEFT
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_choice.CORE with type ('a, 'b) t = ('a, 'b) Req.t = struct
  module C = Arrow.Core_over_category_and_via_arrow_and_split (Category) (Req)
  include C

  let left = Req.left

  include Choose_over_left (Category) (C) (Req)
end

module Core_over_category_and_via_arrow_and_fst_and_choose
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_choice.WITH_ARROW_AND_FST_AND_CHOOSE
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_choice.CORE with type ('a, 'b) t = ('a, 'b) Req.t = struct
  include Arrow.Core_over_category_and_via_arrow_and_fst (Category) (Req)

  let choose = Req.choose

  include Left_over_choose (Category) (Req)
end

module Core_over_category_and_via_arrow_and_split_and_choose
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_choice.WITH_ARROW_AND_SPLIT_AND_CHOOSE
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_choice.CORE with type ('a, 'b) t = ('a, 'b) Req.t = struct
  include Arrow.Core_over_category_and_via_arrow_and_split (Category) (Req)

  let choose = Req.choose

  include Left_over_choose (Category) (Req)
end

module Operation_over_category
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_choice.CORE
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_choice.OPERATION with type ('a, 'b) t = ('a, 'b) Core.t =
struct
  include Arrow.Operation_over_category (Category) (Core)

  let right x = Core.choose Category.id x

  let fan_in f g = Category.(Core.choose f g >>> Core.arrow extract)
end

module Alias = Arrow.Alias

module Infix_over_category
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_choice.CORE
              with type ('a, 'b) t = ('a, 'b) Category.t)
    (Operation : Preface_specs.Arrow_choice.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) :
  Preface_specs.Arrow_choice.INFIX with type ('a, 'b) t = ('a, 'b) Core.t =
struct
  include Arrow.Infix_over_category (Category) (Core) (Operation)

  let ( +++ ) = Core.choose

  let ( ||| ) = Operation.fan_in
end

module Via
    (Core : Preface_specs.Arrow_choice.CORE)
    (Operation : Preface_specs.Arrow_choice.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t)
    (Alias : Preface_specs.Arrow_choice.ALIAS
               with type ('a, 'b) t = ('a, 'b) Operation.t)
    (Infix : Preface_specs.Arrow_choice.INFIX
               with type ('a, 'b) t = ('a, 'b) Alias.t) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) Infix.t = struct
  include Core
  include Operation
  include Alias
  include Infix
  module Infix = Infix
end

module Over_category_and_via_arrow_and_fst_and_left
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_choice.WITH_ARROW_AND_FST_AND_LEFT
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) Req.t = struct
  module Core =
    Core_over_category_and_via_arrow_and_fst_and_left (Category) (Req)
  module Operation = Operation_over_category (Category) (Core)
  module Alias = Alias (Operation)
  module Infix = Infix_over_category (Category) (Core) (Operation)
  include Core
  include Operation
  include Alias
  include Infix
end

module Over_over_category_and_via_arrow_and_split_and_left
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_choice.WITH_ARROW_AND_SPLIT_AND_LEFT
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) Req.t = struct
  module Core =
    Core_over_category_and_via_arrow_and_split_and_left (Category) (Req)
  module Operation = Operation_over_category (Category) (Core)
  module Alias = Alias (Operation)
  module Infix = Infix_over_category (Category) (Core) (Operation)
  include Core
  include Operation
  include Alias
  include Infix
end

module Over_category_and_via_arrow_and_fst_and_choose
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_choice.WITH_ARROW_AND_FST_AND_CHOOSE
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) Req.t = struct
  module Core =
    Core_over_category_and_via_arrow_and_fst_and_choose (Category) (Req)
  module Operation = Operation_over_category (Category) (Core)
  module Alias = Alias (Operation)
  module Infix = Infix_over_category (Category) (Core) (Operation)
  include Core
  include Operation
  include Alias
  include Infix
end

module Over_category_and_via_arrow_and_split_and_choose
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_choice.WITH_ARROW_AND_SPLIT_AND_CHOOSE
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) Req.t = struct
  module Core =
    Core_over_category_and_via_arrow_and_split_and_choose (Category) (Req)
  module Operation = Operation_over_category (Category) (Core)
  module Alias = Alias (Operation)
  module Infix = Infix_over_category (Category) (Core) (Operation)
  include Core
  include Operation
  include Alias
  include Infix
end

module Over_arrow_with_left
    (Arrow : Preface_specs.ARROW)
    (Left : Preface_specs.Arrow_choice.WITH_LEFT
              with type ('a, 'b) t = ('a, 'b) Arrow.t) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) Left.t = struct
  module Core_aux =
    Core_over_category_and_via_arrow_and_fst_and_left
      (Arrow)
      (struct
        include Arrow
        include Left
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

module Over_arrow_with_choose
    (Arrow : Preface_specs.ARROW)
    (Choose : Preface_specs.Arrow_choice.WITH_CHOOSE
                with type ('a, 'b) t = ('a, 'b) Arrow.t) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) Choose.t = struct
  module Core_aux =
    Core_over_category_and_via_arrow_and_fst_and_choose
      (Arrow)
      (struct
        include Arrow
        include Choose
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

module Over_arrow_with_left_and_choose
    (Arrow : Preface_specs.ARROW)
    (Choose_left : Preface_specs.Arrow_choice.WITH_LEFT_AND_CHOOSE
                     with type ('a, 'b) t = ('a, 'b) Arrow.t) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) Choose_left.t =
struct
  module Core_aux = struct
    include Arrow
    include Choose_left
  end

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

module From_monad (Monad : Preface_specs.Monad.CORE) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = 'a -> 'b Monad.t = struct
  module Arr = Arrow.From_monad (Monad)

  include
    Over_arrow_with_choose
      (Arr)
      (struct
        type ('a, 'b) t = 'a -> 'b Monad.t

        let choose f g =
          let left = Arr.(f >>> arrow Either.left)
          and right = Arr.(g >>> arrow Either.right) in
          Preface_core.Shims.Either.case left right
        ;;
      end)
end

module Product (F : Preface_specs.ARROW_CHOICE) (G : Preface_specs.ARROW_CHOICE) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) F.t * ('a, 'b) G.t =
  Over_arrow_with_left
    (Arrow.Product (F) (G))
       (struct
         type ('a, 'b) t = ('a, 'b) F.t * ('a, 'b) G.t

         let left (x, y) = (F.left x, G.left y)
       end)
