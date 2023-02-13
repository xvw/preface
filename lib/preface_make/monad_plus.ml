module Core_via_bind (Req : Preface_specs.Monad_plus.WITH_BIND) = struct
  type 'a t = 'a Req.t

  include (
    Indexed_monad_plus.Core_via_bind (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req : Preface_specs.Monad_plus.WITH_BIND with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_monad_plus.CORE with type ('a, _) t := 'a Req.t )
end

module Core_via_map_and_join (Req : Preface_specs.Monad_plus.WITH_MAP_AND_JOIN) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_monad_plus.Core_via_map_and_join (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req :
          Preface_specs.Monad_plus.WITH_MAP_AND_JOIN with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_monad_plus.CORE with type ('a, _) t := 'a Req.t )
end

module Core_via_kleisli_composition
    (Req : Preface_specs.Monad_plus.WITH_KLEISLI_COMPOSITION) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_monad_plus.Core_via_kleisli_composition (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req :
          Preface_specs.Monad_plus.WITH_KLEISLI_COMPOSITION
            with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_monad_plus.CORE with type ('a, _) t := 'a Req.t )
end

module Operation (Core : Preface_specs.Monad_plus.CORE) = struct
  type 'a t = 'a Core.t

  include (
    Indexed_monad_plus.Operation (struct
      type ('a, 'index) t = 'a Core.t

      include (Core : Preface_specs.Monad_plus.CORE with type 'a t := 'a Core.t)
    end) :
      Preface_specs.Indexed_monad_plus.OPERATION
        with type ('a, _) t := 'a Core.t )
end

module Syntax (Core : Preface_specs.Monad_plus.CORE) = struct
  type 'a t = 'a Core.t

  include (
    Indexed_monad_plus.Syntax (struct
      type ('a, 'index) t = 'a Core.t

      include (Core : Preface_specs.Monad_plus.CORE with type 'a t := 'a Core.t)
    end) :
      Preface_specs.Indexed_monad_plus.SYNTAX with type ('a, _) t := 'a Core.t )
end

module Infix
    (Core : Preface_specs.Monad_plus.CORE)
    (Operation : Preface_specs.Monad_plus.OPERATION with type 'a t = 'a Core.t) =
struct
  type 'a t = 'a Core.t

  include (
    Indexed_monad_plus.Infix
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Core : Preface_specs.Monad_plus.CORE with type 'a t := 'a Core.t )
      end)
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Operation :
            Preface_specs.Monad_plus.OPERATION with type 'a t := 'a Core.t )
      end) :
      Preface_specs.Indexed_monad_plus.INFIX with type ('a, _) t := 'a Core.t )
end

module Via
    (Core : Preface_specs.Monad_plus.CORE)
    (Operation : Preface_specs.Monad_plus.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Monad_plus.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Monad_plus.SYNTAX with type 'a t = 'a Core.t) =
struct
  type 'a t = 'a Core.t

  include (
    Indexed_monad_plus.Via
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Core : Preface_specs.Monad_plus.CORE with type 'a t := 'a Core.t )
      end)
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Operation :
            Preface_specs.Monad_plus.OPERATION with type 'a t := 'a Core.t )
      end)
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Infix : Preface_specs.Monad_plus.INFIX with type 'a t := 'a Core.t )
      end)
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Syntax : Preface_specs.Monad_plus.SYNTAX with type 'a t := 'a Core.t )
      end) :
      Preface_specs.Indexed_monad_plus.API with type ('a, _) t := 'a Core.t )
end

module Via_bind (Req : Preface_specs.Monad_plus.WITH_BIND) = struct
  type 'a t = 'a Req.t

  include (
    Indexed_monad_plus.Via_bind (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req : Preface_specs.Monad_plus.WITH_BIND with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_monad_plus.API with type ('a, _) t := 'a Req.t )
end

module Via_map_and_join (Req : Preface_specs.Monad_plus.WITH_MAP_AND_JOIN) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_monad_plus.Via_map_and_join (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req :
          Preface_specs.Monad_plus.WITH_MAP_AND_JOIN with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_monad_plus.API with type ('a, _) t := 'a Req.t )
end

module Via_kleisli_composition
    (Req : Preface_specs.Monad_plus.WITH_KLEISLI_COMPOSITION) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_monad_plus.Via_kleisli_composition (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req :
          Preface_specs.Monad_plus.WITH_KLEISLI_COMPOSITION
            with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_monad_plus.API with type ('a, _) t := 'a Req.t )
end

module Over_monad
    (M : Preface_specs.MONAD)
    (Req : Preface_specs.Monad_plus.WITH_NEUTRAL_AND_COMBINE
             with type 'a t = 'a M.t) =
struct
  type 'a t = 'a M.t

  include (
    Indexed_monad_plus.Over_monad
      (Monad.Index
         (M))
         (struct
           type ('a, 'index) t = 'a M.t

           include (
             Req :
               Preface_specs.Monad_plus.WITH_NEUTRAL_AND_COMBINE
                 with type 'a t := 'a M.t )
         end) :
        Preface_specs.INDEXED_MONAD_PLUS with type ('a, _) t := 'a M.t )
end

module Over_monad_and_alternative
    (Monad : Preface_specs.MONAD)
    (Alternative : Preface_specs.ALTERNATIVE with type 'a t = 'a Monad.t) =
  Over_monad (Monad) (Alternative)

module From_arrow_apply_and_arrow_plus
    (A : Preface_specs.ARROW_APPLY)
    (P : Preface_specs.ARROW_PLUS with type ('a, 'b) t = ('a, 'b) A.t) =
  Over_monad_and_alternative
    (Monad.From_arrow_apply (A)) (Alternative.From_arrow_plus (P))

module Product (F : Preface_specs.MONAD_PLUS) (G : Preface_specs.MONAD_PLUS) =
  Over_monad
    (Monad.Product (F) (G))
       (struct
         type 'a t = 'a F.t * 'a G.t

         let neutral = (F.neutral, G.neutral)
         let combine (x1, y1) (x2, y2) = (F.combine x1 x2, G.combine y1 y2)
       end)

module Index (F : Preface_specs.MONAD_PLUS) = struct
  type ('a, 'index) t = 'a F.t

  include (
    Indexed_monad_plus.Via
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F : Preface_specs.Monad_plus.CORE with type 'a t := 'a F.t)
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F : Preface_specs.Monad_plus.OPERATION with type 'a t := 'a F.t)
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (
          F.Infix : Preface_specs.Monad_plus.INFIX with type 'a t := 'a F.t )
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (
          F.Syntax : Preface_specs.Monad_plus.SYNTAX with type 'a t := 'a F.t )
      end) :
      Preface_specs.INDEXED_MONAD_PLUS
        with type ('a, 'index) t := ('a, 'index) t )
end
