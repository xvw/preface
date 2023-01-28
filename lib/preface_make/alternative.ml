module Core_via_pure_map_and_product
    (Req : Preface_specs.Alternative.WITH_PURE_MAP_AND_PRODUCT) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_alternative.Core_via_pure_map_and_product (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req :
          Preface_specs.Alternative.WITH_PURE_MAP_AND_PRODUCT
            with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_alternative.CORE with type ('a, _) t := 'a Req.t )
end

module Core_via_pure_and_apply
    (Req : Preface_specs.Alternative.WITH_PURE_AND_APPLY) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_alternative.Core_via_pure_and_apply (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req :
          Preface_specs.Alternative.WITH_PURE_AND_APPLY
            with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_alternative.CORE with type ('a, _) t := 'a Req.t )
end

module Core_via_pure_and_lift2
    (Req : Preface_specs.Alternative.WITH_PURE_AND_LIFT2) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_alternative.Core_via_pure_and_lift2 (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req :
          Preface_specs.Alternative.WITH_PURE_AND_LIFT2
            with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_alternative.CORE with type ('a, _) t := 'a Req.t )
end

module Operation (Core : Preface_specs.Alternative.CORE) = struct
  type 'a t = 'a Core.t

  include (
    Indexed_alternative.Operation (struct
      type ('a, 'index) t = 'a Core.t

      include (Core : Preface_specs.Alternative.CORE with type 'a t := 'a Core.t)
    end) :
      Preface_specs.Indexed_alternative.OPERATION
        with type ('a, _) t := 'a Core.t )
end

module Syntax (Core : Preface_specs.Alternative.CORE) = struct
  type 'a t = 'a Core.t

  include (
    Indexed_alternative.Syntax (struct
      type ('a, 'index) t = 'a Core.t

      include (Core : Preface_specs.Alternative.CORE with type 'a t := 'a Core.t)
    end) :
      Preface_specs.Indexed_alternative.SYNTAX with type ('a, _) t := 'a Core.t )
end

module Infix
    (Core : Preface_specs.Alternative.CORE)
    (Operation : Preface_specs.Alternative.OPERATION with type 'a t = 'a Core.t) =
struct
  type 'a t = 'a Core.t

  include (
    Indexed_alternative.Infix
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Core : Preface_specs.Alternative.CORE with type 'a t := 'a Core.t )
      end)
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Operation :
            Preface_specs.Alternative.OPERATION with type 'a t := 'a Operation.t )
      end) :
      Preface_specs.Indexed_alternative.INFIX with type ('a, _) t := 'a Core.t )
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

module Via_pure_map_and_product
    (Req : Preface_specs.Alternative.WITH_PURE_MAP_AND_PRODUCT) =
struct
  module Core = Core_via_pure_map_and_product (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_pure_and_apply (Req : Preface_specs.Alternative.WITH_PURE_AND_APPLY) =
struct
  module Core = Core_via_pure_and_apply (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_pure_and_lift2 (Req : Preface_specs.Alternative.WITH_PURE_AND_LIFT2) =
struct
  module Core = Core_via_pure_and_lift2 (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

(* FIXME: find a way to perform module strengthening inside submodules.*)
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
      let reduce list = List.fold_left Req.combine Req.neutral list
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

module Index (F : Preface_specs.ALTERNATIVE) = struct
  type ('a, 'index) t = 'a F.t

  include (
    Indexed_alternative.Via
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F : Preface_specs.Alternative.CORE with type 'a t := 'a F.t)
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (
          F : Preface_specs.Alternative.OPERATION with type 'a t := 'a F.t )
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (
          F.Infix : Preface_specs.Alternative.INFIX with type 'a t := 'a F.t )
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (
          F.Syntax : Preface_specs.Alternative.SYNTAX with type 'a t := 'a F.t )
      end) :
      Preface_specs.INDEXED_ALTERNATIVE
        with type ('a, 'index) t := ('a, 'index) t )
end
