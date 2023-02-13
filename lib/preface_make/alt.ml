module Core_over_functor
    (F : Preface_specs.FUNCTOR)
    (Req : Preface_specs.Alt.WITH_COMBINE with type 'a t = 'a F.t) =
struct
  type 'a t = 'a F.t

  include (
    Indexed_alt.Core_over_functor
      (Functor.Index
         (F))
         (struct
           type ('a, 'index) t = 'a Req.t

           include (
             Req : Preface_specs.Alt.WITH_COMBINE with type 'a t := 'a Req.t )
         end) :
        Preface_specs.Indexed_alt.CORE with type ('a, _) t := 'a F.t )
end

module Core (Req : Preface_specs.Alt.WITH_COMBINE_AND_MAP) = Req

module Operation (Core : Preface_specs.Alt.CORE) = struct
  type 'a t = 'a Core.t

  include (
    Indexed_alt.Operation (struct
      type ('a, 'index) t = 'a Core.t

      include (Core : Preface_specs.Alt.CORE with type 'a t := 'a Core.t)
    end) :
      Preface_specs.Indexed_alt.OPERATION with type ('a, _) t := 'a Core.t )
end

module Infix
    (Core : Preface_specs.Alt.CORE)
    (Operation : Preface_specs.Alt.OPERATION with type 'a t = 'a Core.t) =
struct
  type 'a t = 'a Core.t

  include (
    Indexed_alt.Infix
      (struct
        type ('a, 'index) t = 'a Core.t

        include (Core : Preface_specs.Alt.CORE with type 'a t := 'a Core.t)
      end)
      (struct
        type ('a, 'index) t = 'a Operation.t

        include (
          Operation : Preface_specs.Alt.OPERATION with type 'a t := 'a Core.t )
      end) :
      Preface_specs.Indexed_alt.INFIX with type ('a, _) t := 'a Core.t )
end

module Syntax (Core : Preface_specs.Alt.CORE) = struct
  type 'a t = 'a Core.t

  include (
    Indexed_alt.Syntax (struct
      type ('a, 'index) t = 'a Core.t

      include (Core : Preface_specs.Alt.CORE with type 'a t := 'a Core.t)
    end) :
      Preface_specs.Indexed_alt.SYNTAX with type ('a, _) t := 'a Core.t )
end

module Via
    (Core : Preface_specs.Alt.CORE)
    (Operation : Preface_specs.Alt.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Alt.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Alt.SYNTAX with type 'a t = 'a Core.t) =
struct
  type 'a t = 'a Core.t

  include (
    Indexed_alt.Via
      (struct
        type ('a, 'index) t = 'a Core.t

        include (Core : Preface_specs.Alt.CORE with type 'a t := 'a Core.t)
      end)
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Operation : Preface_specs.Alt.OPERATION with type 'a t := 'a Core.t )
      end)
      (struct
        type ('a, 'index) t = 'a Core.t

        include (Infix : Preface_specs.Alt.INFIX with type 'a t := 'a Core.t)
      end)
      (struct
        type ('a, 'index) t = 'a Core.t

        include (Syntax : Preface_specs.Alt.SYNTAX with type 'a t := 'a Core.t)
      end) :
      Preface_specs.Indexed_alt.API with type ('a, _) t := 'a Core.t )
end

module Via_map_and_combine (Req : Preface_specs.Alt.WITH_COMBINE_AND_MAP) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_alt.Via_map_and_combine (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req : Preface_specs.Alt.WITH_COMBINE_AND_MAP with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_alt.API with type ('a, _) t := 'a Req.t )
end

module Over_functor
    (F : Preface_specs.FUNCTOR)
    (Combine : Preface_specs.Alt.WITH_COMBINE with type 'a t = 'a F.t) =
struct
  type 'a t = 'a F.t

  include (
    Indexed_alt.Over_functor
      (struct
        type ('a, 'index) t = 'a F.t

        include (F : Preface_specs.FUNCTOR with type 'a t := 'a F.t)
      end)
      (struct
        type ('a, 'index) t = 'a F.t

        include (
          Combine : Preface_specs.Alt.WITH_COMBINE with type 'a t := 'a F.t )
      end) :
      Preface_specs.Indexed_alt.API with type ('a, _) t := 'a F.t )
end

module Composition (F : Preface_specs.ALT) (G : Preface_specs.FUNCTOR) =
  Over_functor
    (Functor.Composition (F) (G))
       (struct
         type 'a t = 'a G.t F.t

         let combine x y = F.combine x y
       end)

module Product (F : Preface_specs.ALT) (G : Preface_specs.ALT) =
  Over_functor
    (Functor.Product (F) (G))
       (struct
         type 'a t = 'a F.t * 'a G.t

         let combine (x1, y1) (x2, y2) = (F.combine x1 x2, G.combine y1 y2)
       end)

module Index (F : Preface_specs.ALT) = struct
  type ('a, 'index) t = 'a F.t

  include (
    Indexed_alt.Via
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F : Preface_specs.Alt.CORE with type 'a t := 'a F.t)
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F : Preface_specs.Alt.OPERATION with type 'a t := 'a F.t)
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F.Infix : Preface_specs.Alt.INFIX with type 'a t := 'a F.t)
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F.Syntax : Preface_specs.Alt.SYNTAX with type 'a t := 'a F.t)
      end) :
      Preface_specs.INDEXED_ALT with type ('a, 'index) t := ('a, 'index) t )
end
