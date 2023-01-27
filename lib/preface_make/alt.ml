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
    (Operation : Preface_specs.Alt.OPERATION)
    (Infix : Preface_specs.Alt.INFIX)
    (Syntax : Preface_specs.Alt.SYNTAX) =
struct
  include Core
  include Operation
  module Infix = Infix
  include Infix
  module Syntax = Syntax
  include Syntax
end

module Via_map_and_combine (Req : Preface_specs.Alt.WITH_COMBINE_AND_MAP) =
struct
  module Core = Core (Req)
  include Core
  module Operation = Operation (Core)
  module Infix = Infix (Core) (Operation)
  module Syntax = Syntax (Core)
  include Operation
  include Infix
  include Syntax
end

module Over_functor
    (F : Preface_specs.FUNCTOR)
    (Combine : Preface_specs.Alt.WITH_COMBINE with type 'a t = 'a F.t) =
struct
  module Core' = Core_over_functor (F) (Combine)
  module Operation' = Operation (Core')

  module Infix' = struct
    include Infix (Core') (Operation')
    include F.Infix
  end

  module Syntax' = struct
    include F.Syntax
  end

  include Core'
  include Operation'
  include F
  module Infix = Infix'
  include Infix
  module Syntax = Syntax'
  include Syntax
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
