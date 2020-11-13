module Core_over_category_and_via_arrow_and_fst
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_plus.CORE_WITH_ARROW_AND_FST
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_plus.CORE with type ('a, 'b) t = ('a, 'b) Core.t = struct
  include Arrow.Core_over_category_and_via_arrow_and_fst (Category) (Core)

  let combine = Core.combine

  let neutral = Core.neutral
end

module Core_over_category_and_via_arrow_and_split
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_plus.CORE_WITH_ARROW_AND_SPLIT
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_plus.CORE with type ('a, 'b) t = ('a, 'b) Core.t = struct
  include Arrow.Core_over_category_and_via_arrow_and_split (Category) (Core)

  let combine = Core.combine

  let neutral = Core.neutral
end

let times' combine n x =
  if n > 0
  then
    let result = Array.make (pred n) x |> Array.fold_left combine x in
    Some result
  else None
;;

let reduce_nel' combine list = Preface_core.Nonempty_list.reduce combine list

let reduce' combine neutral list = List.fold_left combine neutral list

module Operation_over_category
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_plus.CORE
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_plus.OPERATION with type ('a, 'b) t = ('a, 'b) Core.t =
struct
  include Arrow.Operation_over_category (Category) (Core)

  let times n x = times' Core.combine n x

  let reduce_nel list = reduce_nel' Core.combine list

  let reduce list = reduce' Core.combine Core.neutral list
end

module Alias = Arrow.Alias

module Infix_over_category
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_plus.CORE
              with type ('a, 'b) t = ('a, 'b) Category.t)
    (Operation : Preface_specs.Arrow_plus.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) :
  Preface_specs.Arrow_plus.INFIX with type ('a, 'b) t = ('a, 'b) Operation.t =
struct
  include Arrow.Infix_over_category (Category) (Core) (Operation)

  let ( <|> ) = Core.combine
end

module Via
    (Core : Preface_specs.Arrow_plus.CORE)
    (Operation : Preface_specs.Arrow_plus.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t)
    (Alias : Preface_specs.Arrow_plus.ALIAS
               with type ('a, 'b) t = ('a, 'b) Operation.t)
    (Infix : Preface_specs.Arrow_plus.INFIX
               with type ('a, 'b) t = ('a, 'b) Alias.t) :
  Preface_specs.ARROW_PLUS with type ('a, 'b) t = ('a, 'b) Infix.t = struct
  include Core
  include Operation
  include Alias
  include Infix
  module Infix = Infix
end

module Over_category_and_via_arrow_and_fst
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_plus.CORE_WITH_ARROW_AND_FST
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_PLUS with type ('a, 'b) t = ('a, 'b) Core.t = struct
  module Core = Core_over_category_and_via_arrow_and_fst (Category) (Core)
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
    (Core : Preface_specs.Arrow_plus.CORE_WITH_ARROW_AND_SPLIT
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_PLUS with type ('a, 'b) t = ('a, 'b) Core.t = struct
  module Core = Core_over_category_and_via_arrow_and_split (Category) (Core)
  module Operation = Operation_over_category (Category) (Core)
  module Alias = Alias (Operation)
  module Infix = Infix_over_category (Category) (Core) (Operation)
  include Core
  include Operation
  include Alias
  include Infix
end

module From_monad_plus (Monad : Preface_specs.Monad_plus.CORE) :
  Preface_specs.ARROW_PLUS with type ('a, 'b) t = 'a -> 'b Monad.t = struct
  module Cat = Category.From_monad (Monad)

  module Arr = struct
    type ('a, 'b) t = 'a -> 'b Monad.t

    let arrow f = (fun f g x -> f (g x)) Monad.return f

    let fst f (b, d) = Monad.bind (fun c -> Monad.return (c, d)) (f b)

    let combine f g x = Monad.combine (f x) (g x)

    let neutral _ = Monad.neutral
  end

  include Over_category_and_via_arrow_and_fst (Cat) (Arr)
end

module Over_arrow
    (Arrow : Preface_specs.ARROW)
    (Combine_and_neutral : Preface_specs.Arrow_plus.COMBINE_AND_NEUTRAL
                             with type ('a, 'b) t = ('a, 'b) Arrow.t) :
  Preface_specs.ARROW_PLUS with type ('a, 'b) t = ('a, 'b) Combine_and_neutral.t =
struct
  include Arrow

  include (
    Combine_and_neutral :
      Preface_specs.Arrow_plus.COMBINE_AND_NEUTRAL
        with type ('a, 'b) t := ('a, 'b) t )

  let times n x = times' Combine_and_neutral.combine n x

  let reduce_nel list = reduce_nel' Combine_and_neutral.combine list

  let reduce list =
    reduce' Combine_and_neutral.combine Combine_and_neutral.neutral list
  ;;

  module Infix = struct
    include Arrow.Infix

    let ( <|> ) = Combine_and_neutral.combine
  end

  include Infix
end
