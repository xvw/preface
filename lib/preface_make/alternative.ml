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
    (Operation : Preface_specs.Alternative.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Alternative.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Alternative.SYNTAX with type 'a t = 'a Core.t) =
struct
  type 'a t = 'a Core.t

  include (
    Indexed_alternative.Via
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Core : Preface_specs.Alternative.CORE with type 'a t := 'a Core.t )
      end)
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Operation :
            Preface_specs.Alternative.OPERATION with type 'a t := 'a Core.t )
      end)
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Infix : Preface_specs.Alternative.INFIX with type 'a t := 'a Core.t )
      end)
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Syntax : Preface_specs.Alternative.SYNTAX with type 'a t := 'a Core.t )
      end) :
      Preface_specs.Indexed_alternative.API with type ('a, _) t := 'a Core.t )
end

module Via_pure_map_and_product
    (Req : Preface_specs.Alternative.WITH_PURE_MAP_AND_PRODUCT) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_alternative.Via_pure_map_and_product (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req :
          Preface_specs.Alternative.WITH_PURE_MAP_AND_PRODUCT
            with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_alternative.API with type ('a, _) t := 'a Req.t )
end

module Via_pure_and_apply (Req : Preface_specs.Alternative.WITH_PURE_AND_APPLY) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_alternative.Via_pure_and_apply (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req :
          Preface_specs.Alternative.WITH_PURE_AND_APPLY
            with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_alternative.API with type ('a, _) t := 'a Req.t )
end

module Via_pure_and_lift2 (Req : Preface_specs.Alternative.WITH_PURE_AND_LIFT2) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_alternative.Via_pure_and_lift2 (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req :
          Preface_specs.Alternative.WITH_PURE_AND_LIFT2
            with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_alternative.API with type ('a, _) t := 'a Req.t )
end

module Over_applicative
    (A : Preface_specs.APPLICATIVE)
    (Req : Preface_specs.Alternative.WITH_NEUTRAL_AND_COMBINE
             with type 'a t = 'a A.t) =
struct
  type 'a t = 'a A.t

  include (
    Indexed_alternative.Over_applicative
      (Applicative.Index
         (A))
         (struct
           type ('a, 'index) t = 'a A.t

           include (
             Req :
               Preface_specs.Alternative.WITH_NEUTRAL_AND_COMBINE
                 with type 'a t := 'a A.t )
         end) :
        Preface_specs.INDEXED_ALTERNATIVE with type ('a, _) t := 'a A.t )
end

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
