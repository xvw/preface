module Core (Req : Preface_specs.Functor.WITH_MAP) = Req

module Operation (Core : Preface_specs.Functor.CORE) = struct
  type 'a t = 'a Core.t

  include (
    Indexed_functor.Operation (struct
      type ('a, 'index) t = 'a Core.t

      include (Core : Preface_specs.Functor.CORE with type 'a t := 'a Core.t)
    end) :
      Preface_specs.Indexed_functor.OPERATION with type ('a, _) t := 'a Core.t )
end

module Infix
    (Core : Preface_specs.Functor.CORE)
    (Operation : Preface_specs.Functor.OPERATION with type 'a t = 'a Core.t) =
struct
  type 'a t = 'a Core.t

  include (
    Indexed_functor.Infix
      (struct
        type ('a, 'index) t = 'a Core.t

        include (Core : Preface_specs.Functor.CORE with type 'a t := 'a Core.t)
      end)
      (struct
        type ('a, 'index) t = 'a Operation.t

        include (
          Operation :
            Preface_specs.Functor.OPERATION with type 'a t := 'a Core.t )
      end) :
      Preface_specs.Indexed_functor.INFIX with type ('a, _) t := 'a Core.t )
end

module Syntax (Core : Preface_specs.Functor.CORE) = struct
  type 'a t = 'a Core.t

  include (
    Indexed_functor.Syntax (struct
      type ('a, 'index) t = 'a Core.t

      include (Core : Preface_specs.Functor.CORE with type 'a t := 'a Core.t)
    end) :
      Preface_specs.Indexed_functor.SYNTAX with type ('a, _) t := 'a Core.t )
end

module Via
    (Core : Preface_specs.Functor.CORE)
    (Operation : Preface_specs.Functor.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Functor.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Functor.SYNTAX with type 'a t = 'a Core.t) =
struct
  type 'a t = 'a Core.t

  include (
    Indexed_functor.Via
      (struct
        type ('a, 'index) t = 'a Core.t

        include (Core : Preface_specs.Functor.CORE with type 'a t := 'a Core.t)
      end)
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Operation :
            Preface_specs.Functor.OPERATION with type 'a t := 'a Core.t )
      end)
      (struct
        type ('a, 'index) t = 'a Core.t

        include (Infix : Preface_specs.Functor.INFIX with type 'a t := 'a Core.t)
      end)
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Syntax : Preface_specs.Functor.SYNTAX with type 'a t := 'a Core.t )
      end) :
      Preface_specs.Indexed_functor.API with type ('a, _) t := 'a Core.t )
end

module Via_map (Req : Preface_specs.Functor.WITH_MAP) = struct
  type 'a t = 'a Req.t

  include (
    Indexed_functor.Via_map (struct
      type ('a, 'index) t = 'a Req.t

      include (Req : Preface_specs.Functor.WITH_MAP with type 'a t := 'a Req.t)
    end) :
      Preface_specs.Indexed_functor.API with type ('a, _) t := 'a Req.t )
end

module Composition (F : Preface_specs.FUNCTOR) (G : Preface_specs.FUNCTOR) =
Via_map (struct
  type 'a t = 'a G.t F.t

  let map f x = F.map (G.map f) x
end)

module From_arrow (A : Preface_specs.ARROW) = Via_map (struct
  type 'a t = (unit, 'a) A.t

  let map f x = A.(x >>> arrow f)
end)

module From_applicative (Applicative : Preface_specs.APPLICATIVE) = Applicative
module From_alt (Alt : Preface_specs.ALT) = Alt
module From_monad (Monad : Preface_specs.MONAD) = Monad
module From_alternative (Alternative : Preface_specs.ALTERNATIVE) = Alternative
module From_monad_plus (Monad_plus : Preface_specs.MONAD_PLUS) = Monad_plus
module From_comonad (Comonad : Preface_specs.COMONAD) = Comonad

module From_bifunctor (Bifunctor : Preface_specs.Bifunctor.CORE) =
Via_map (struct
  type 'a t = ('a, 'a) Bifunctor.t

  let map f b = Bifunctor.bimap f f b
end)

module Sum (F : Preface_specs.FUNCTOR) (G : Preface_specs.FUNCTOR) = struct
  type 'a sum =
    | L of 'a F.t
    | R of 'a G.t

  include Via_map (struct
    type 'a t = 'a sum

    let map f = function L x -> L (F.map f x) | R x -> R (G.map f x)
  end)
end

module Product (F : Preface_specs.FUNCTOR) (G : Preface_specs.FUNCTOR) =
Via_map (struct
  type 'a t = 'a F.t * 'a G.t

  let map f (x, y) = (F.map f x, G.map f y)
end)

module Index (F : Preface_specs.FUNCTOR) = struct
  type ('a, 'index) t = 'a F.t

  include (
    Indexed_functor.Via
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F : Preface_specs.Functor.CORE with type 'a t := 'a F.t)
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F : Preface_specs.Functor.OPERATION with type 'a t := 'a F.t)
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F.Infix : Preface_specs.Functor.INFIX with type 'a t := 'a F.t)
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (
          F.Syntax : Preface_specs.Functor.SYNTAX with type 'a t := 'a F.t )
      end) :
      Preface_specs.INDEXED_FUNCTOR with type ('a, 'index) t := ('a, 'index) t )
end
