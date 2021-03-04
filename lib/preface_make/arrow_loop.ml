module Core_over_category_and_via_arrow_and_fst_and_loop
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_loop.CORE_WITH_ARROW_AND_FST_AND_LOOP
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_loop.CORE with type ('a, 'b) t = ('a, 'b) Core.t = struct
  include Arrow.Core_over_category_and_via_arrow_and_fst (Category) (Core)

  let loop = Core.loop
end

module Core_over_category_and_via_arrow_and_split_and_loop
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_loop.CORE_WITH_ARROW_AND_SPLIT_AND_LOOP
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_loop.CORE with type ('a, 'b) t = ('a, 'b) Core.t = struct
  include Arrow.Core_over_category_and_via_arrow_and_split (Category) (Core)

  let loop = Core.loop
end

module Operation_over_category = Arrow.Operation_over_category
module Infix_over_category = Arrow.Infix_over_category
module Alias = Arrow.Alias

module Via
    (Core : Preface_specs.Arrow_loop.CORE)
    (Operation : Preface_specs.Arrow_loop.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t)
    (Alias : Preface_specs.Arrow_loop.ALIAS
               with type ('a, 'b) t = ('a, 'b) Operation.t)
    (Infix : Preface_specs.Arrow_loop.INFIX
               with type ('a, 'b) t = ('a, 'b) Alias.t) :
  Preface_specs.ARROW_LOOP with type ('a, 'b) t = ('a, 'b) Infix.t = struct
  include Core
  include Operation
  include Alias
  include Infix
  module Infix = Infix
end

module Over_category_and_via_arrow_and_fst_and_loop
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_loop.CORE_WITH_ARROW_AND_FST_AND_LOOP
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_LOOP with type ('a, 'b) t = ('a, 'b) Core.t = struct
  module Core =
    Core_over_category_and_via_arrow_and_fst_and_loop (Category) (Core)
  module Operation = Operation_over_category (Category) (Core)
  module Alias = Alias (Operation)
  module Infix = Infix_over_category (Category) (Core) (Operation)
  include Core
  include Operation
  include Alias
  include Infix
end

module Over_category_and_via_arrow_and_split_and_loop
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_loop.CORE_WITH_ARROW_AND_SPLIT_AND_LOOP
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_LOOP with type ('a, 'b) t = ('a, 'b) Core.t = struct
  module Core =
    Core_over_category_and_via_arrow_and_split_and_loop (Category) (Core)
  module Operation = Operation_over_category (Category) (Core)
  module Alias = Alias (Operation)
  module Infix = Infix_over_category (Category) (Core) (Operation)
  include Core
  include Operation
  include Alias
  include Infix
end

module Over_arrow_with_loop
    (Arrow : Preface_specs.ARROW)
    (Loop : Preface_specs.Arrow_loop.WITH_LOOP
              with type ('a, 'b) t = ('a, 'b) Arrow.t) :
  Preface_specs.ARROW_LOOP with type ('a, 'b) t = ('a, 'b) Loop.t = struct
  module Core_aux =
    Core_over_category_and_via_arrow_and_fst_and_loop
      (Arrow)
      (struct
        include Arrow
        include Loop
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
