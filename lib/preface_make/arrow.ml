module Core_over_category_and_via_arrow_and_fst
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow.CORE_WITH_ARROW_AND_FST
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow.CORE with type ('a, 'b) t = ('a, 'b) Core.t = struct
  include Category
  include Core

  let split pre post =
    let open Infix in
    let swap (x, y) = (y, x) in
    fst pre >>> arrow swap >>> fst post >>> arrow swap
  ;;
end

module Core_over_category_and_via_arrow_and_split
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow.CORE_WITH_ARROW_AND_SPLIT
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow.CORE with type ('a, 'b) t = ('a, 'b) Core.t = struct
  include Category
  include Core

  let fst x = split x id
end

module Operation_over_category
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow.CORE with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow.OPERATION with type ('a, 'b) t = ('a, 'b) Core.t = struct
  include Category

  let return () = Core.arrow (fun x -> x)

  let snd x = Core.split id x

  let fan_out pre post =
    Infix.(Core.arrow (fun x -> (x, x)) >>> Core.split pre post)
  ;;

  let pre_compose_left_to_right f x = Infix.(Core.arrow f >>> x)

  let post_compose_left_to_right x f = Infix.(x >>> Core.arrow f)

  let pre_compose_right_to_left x f = Infix.(x <<< Core.arrow f)

  let post_compose_right_to_left f x = Infix.(Core.arrow f <<< x)
end

module Alias (Operation : Preface_specs.Arrow.OPERATION) :
  Preface_specs.Arrow.ALIAS with type ('a, 'b) t = ('a, 'b) Operation.t = struct
  type ('a, 'b) t = ('a, 'b) Operation.t

  let pre_compose f x = Operation.pre_compose_left_to_right f x

  let post_compose x f = Operation.post_compose_left_to_right x f
end

module Infix_over_category
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow.CORE with type ('a, 'b) t = ('a, 'b) Category.t)
    (Operation : Preface_specs.Arrow.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) :
  Preface_specs.Arrow.INFIX with type ('a, 'b) t = ('a, 'b) Operation.t = struct
  include Category.Infix

  let ( *** ) l r = Core.split l r

  let ( &&& ) l r = Operation.fan_out l r

  let ( ^>> ) l r = Operation.pre_compose_left_to_right l r

  let ( >>^ ) l r = Operation.post_compose_left_to_right l r

  let ( <<^ ) l r = Operation.pre_compose_right_to_left l r

  let ( ^<< ) l r = Operation.post_compose_right_to_left l r
end

module Via
    (Core : Preface_specs.Arrow.CORE)
    (Operation : Preface_specs.Arrow.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t)
    (Alias : Preface_specs.Arrow.ALIAS
               with type ('a, 'b) t = ('a, 'b) Operation.t)
    (Infix : Preface_specs.Arrow.INFIX with type ('a, 'b) t = ('a, 'b) Alias.t) :
  Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) Infix.t = struct
  include Core
  include Operation
  include Alias
  include Infix
  module Infix = Infix
end

module Over_category_and_via_arrow_and_fst
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow.CORE_WITH_ARROW_AND_FST
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) Core.t = struct
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
    (Core : Preface_specs.Arrow.CORE_WITH_ARROW_AND_SPLIT
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) Core.t = struct
  module Core = Core_over_category_and_via_arrow_and_split (Category) (Core)
  module Operation = Operation_over_category (Category) (Core)
  module Alias = Alias (Operation)
  module Infix = Infix_over_category (Category) (Core) (Operation)
  include Core
  include Operation
  include Alias
  include Infix
end

module From_monad (Monad : Preface_specs.Monad.CORE) :
  Preface_specs.ARROW with type ('a, 'b) t = 'a -> 'b Monad.t = struct
  module Cat = Category.From_monad (Monad)

  module Arr = struct
    type ('a, 'b) t = 'a -> 'b Monad.t

    let arrow f = (fun f g x -> f (g x)) Monad.return f

    let fst f (b, d) = Monad.bind (fun c -> Monad.return (c, d)) (f b)
  end

  include Over_category_and_via_arrow_and_fst (Cat) (Arr)
end

module From_arrow_plus (Plus : Preface_specs.ARROW_PLUS) :
  Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) Plus.t =
  Plus

module From_arrow_alt (Alt : Preface_specs.ARROW_ALT) :
  Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) Alt.t =
  Alt

module From_arrow_zero (Zero : Preface_specs.ARROW_ZERO) :
  Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) Zero.t =
  Zero

module From_arrow_choice (Choice : Preface_specs.ARROW_CHOICE) :
  Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) Choice.t =
  Choice
