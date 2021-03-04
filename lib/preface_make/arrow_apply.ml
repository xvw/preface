module Core_over_category_and_via_arrow_and_fst_and_apply
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_apply.CORE_WITH_ARROW_AND_FST_AND_APPLY
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_apply.CORE with type ('a, 'b) t = ('a, 'b) Core.t = struct
  include Arrow.Core_over_category_and_via_arrow_and_fst (Category) (Core)

  let apply = Core.apply
end

module Core_over_category_and_via_arrow_and_split_and_apply
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_apply.CORE_WITH_ARROW_AND_SPLIT_AND_APPLY
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_apply.CORE with type ('a, 'b) t = ('a, 'b) Core.t = struct
  include Arrow.Core_over_category_and_via_arrow_and_split (Category) (Core)

  let apply = Core.apply
end

module Operation_over_category = Arrow.Operation_over_category
module Infix_over_category = Arrow.Infix_over_category
module Alias = Arrow.Alias

module Via
    (Core : Preface_specs.Arrow_apply.CORE)
    (Operation : Preface_specs.Arrow_apply.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t)
    (Alias : Preface_specs.Arrow_apply.ALIAS
               with type ('a, 'b) t = ('a, 'b) Operation.t)
    (Infix : Preface_specs.Arrow_apply.INFIX
               with type ('a, 'b) t = ('a, 'b) Alias.t) :
  Preface_specs.ARROW_APPLY with type ('a, 'b) t = ('a, 'b) Infix.t = struct
  include Core
  include Operation
  include Alias
  include Infix
  module Infix = Infix
end

module Over_category_and_via_arrow_and_fst_and_apply
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_apply.CORE_WITH_ARROW_AND_FST_AND_APPLY
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_APPLY with type ('a, 'b) t = ('a, 'b) Core.t = struct
  module Core =
    Core_over_category_and_via_arrow_and_fst_and_apply (Category) (Core)
  module Operation = Operation_over_category (Category) (Core)
  module Alias = Alias (Operation)
  module Infix = Infix_over_category (Category) (Core) (Operation)
  include Core
  include Operation
  include Alias
  include Infix
end

module Over_category_and_via_arrow_and_split_and_apply
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_apply.CORE_WITH_ARROW_AND_SPLIT_AND_APPLY
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_APPLY with type ('a, 'b) t = ('a, 'b) Core.t = struct
  module Core =
    Core_over_category_and_via_arrow_and_split_and_apply (Category) (Core)
  module Operation = Operation_over_category (Category) (Core)
  module Alias = Alias (Operation)
  module Infix = Infix_over_category (Category) (Core) (Operation)
  include Core
  include Operation
  include Alias
  include Infix
end

module Over_arrow_with_apply
    (Arrow : Preface_specs.ARROW)
    (Apply : Preface_specs.Arrow_apply.WITH_APPLY
               with type ('a, 'b) t = ('a, 'b) Arrow.t) :
  Preface_specs.ARROW_APPLY with type ('a, 'b) t = ('a, 'b) Apply.t = struct
  module Core_aux =
    Core_over_category_and_via_arrow_and_fst_and_apply
      (Arrow)
      (struct
        include Arrow
        include Apply
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

module From_monad (Monad : Preface_specs.Monad.CORE) :
  Preface_specs.ARROW_APPLY with type ('a, 'b) t = 'a -> 'b Monad.t = struct
  module Arr = Arrow.From_monad (Monad)

  include Over_arrow_with_apply
            (Arr)
            (struct
              type ('a, 'b) t = 'a -> 'b Monad.t

              let apply (f, x) = f x
            end)
end
