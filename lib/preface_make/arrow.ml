module Core_over_category_and_via_arrow_and_fst
    (Category : Preface_specs.Category.CORE)
    (Req : Preface_specs.Arrow.WITH_ARROW_AND_FST
             with type ('a, 'b) t = ('a, 'b) Category.t) =
struct
  include Category
  include Req

  let split pre post =
    let swap (x, y) = (y, x) in
    let ( >>> ) f g = compose g f in
    fst pre >>> arrow swap >>> fst post >>> arrow swap
  ;;
end

module Core_over_category_and_via_arrow_and_split
    (Category : Preface_specs.Category.CORE)
    (Req : Preface_specs.Arrow.WITH_ARROW_AND_SPLIT
             with type ('a, 'b) t = ('a, 'b) Category.t) =
struct
  include Category
  include Req

  let fst x = split x id
end

module Operation_over_category
    (Category : Preface_specs.Category.OPERATION)
    (Core : Preface_specs.Arrow.CORE with type ('a, 'b) t = ('a, 'b) Category.t) =
struct
  include Category

  let return () = Core.arrow (fun x -> x)
  let snd x = Core.split Core.id x

  let fan_out pre post =
    compose_left_to_right (Core.arrow (fun x -> (x, x))) (Core.split pre post)
  ;;

  let pre_compose_left_to_right f x = compose_left_to_right (Core.arrow f) x
  let post_compose_left_to_right x f = compose_left_to_right x (Core.arrow f)
  let pre_compose_right_to_left x f = compose_right_to_left x (Core.arrow f)
  let post_compose_right_to_left f x = compose_right_to_left (Core.arrow f) x
end

module Alias (Operation : Preface_specs.Arrow.OPERATION) = struct
  type ('a, 'b) t = ('a, 'b) Operation.t

  let pre_compose f x = Operation.pre_compose_left_to_right f x
  let post_compose x f = Operation.post_compose_left_to_right x f
end

module Infix_over_category
    (Category : Preface_specs.Category.INFIX)
    (Core : Preface_specs.Arrow.CORE)
    (Operation : Preface_specs.Arrow.OPERATION) =
struct
  include Category

  let ( *** ) l r = Core.split l r
  let ( &&& ) l r = Operation.fan_out l r
  let ( ^>> ) l r = Operation.pre_compose_left_to_right l r
  let ( >>^ ) l r = Operation.post_compose_left_to_right l r
  let ( <<^ ) l r = Operation.pre_compose_right_to_left l r
  let ( ^<< ) l r = Operation.post_compose_right_to_left l r
end

module Via
    (Core : Preface_specs.Arrow.CORE)
    (Operation : Preface_specs.Arrow.OPERATION)
    (Alias : Preface_specs.Arrow.ALIAS)
    (Infix : Preface_specs.Arrow.INFIX) =
struct
  include Core
  include Operation
  include Alias
  include Infix
  module Infix = Infix
end

module Over_category_and_via_arrow_and_fst
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow.WITH_ARROW_AND_FST
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
    (Req : Preface_specs.Arrow.WITH_ARROW_AND_SPLIT
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

module From_monad (Monad : Preface_specs.Monad.CORE) = struct
  module Cat = Category.From_monad (Monad)

  module Arr = struct
    type ('a, 'b) t = 'a -> 'b Monad.t

    let arrow f = (fun f g x -> f (g x)) Monad.return f
    let fst f (b, d) = Monad.bind (fun c -> Monad.return (c, d)) (f b)
  end

  include Over_category_and_via_arrow_and_fst (Cat) (Arr)
end

module From_arrow_plus (Plus : Preface_specs.ARROW_PLUS) = Plus
module From_arrow_alt (Alt : Preface_specs.ARROW_ALT) = Alt
module From_arrow_zero (Zero : Preface_specs.ARROW_ZERO) = Zero
module From_arrow_choice (Choice : Preface_specs.ARROW_CHOICE) = Choice
module From_arrow_apply (Apply : Preface_specs.ARROW_APPLY) = Apply

module From_strong_and_category
    (Strong : Preface_specs.Strong.WITH_DIMAP_AND_FST)
    (Category : Preface_specs.CATEGORY with type ('a, 'b) t = ('a, 'b) Strong.t) =
  Over_category_and_via_arrow_and_fst
    (Category)
    (struct
      type ('a, 'b) t = ('a, 'b) Category.t

      let arrow f = Strong.dimap f (fun x -> x) Category.id
      let fst = Strong.fst
    end)

module Product (F : Preface_specs.ARROW) (G : Preface_specs.ARROW) =
  Over_category_and_via_arrow_and_fst
    (Category.Product (F) (G))
       (struct
         type ('a, 'b) t = ('a, 'b) F.t * ('a, 'b) G.t

         let arrow f = (F.arrow f, G.arrow f)
         let fst (x, y) = (F.fst x, G.fst y)
       end)
