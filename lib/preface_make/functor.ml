open Preface_core.Fun
module Core (Req : Preface_specs.Functor.WITH_MAP) = Req

module Operation (Core : Preface_specs.Functor.CORE) = struct
  type 'a t = 'a Core.t

  let replace value x = (Core.map <% const) value x
  let void x = replace () x
end

module Infix
    (Core : Preface_specs.Functor.CORE)
    (Operation : Preface_specs.Functor.OPERATION with type 'a t = 'a Core.t) =
struct
  type 'a t = 'a Operation.t

  let ( <$> ) = Core.map
  let ( <&> ) x f = Core.map f x
  let ( <$ ) value x = Operation.replace value x
  let ( $> ) x value = Operation.replace value x
end

module Via
    (Core : Preface_specs.Functor.CORE)
    (Operation : Preface_specs.Functor.OPERATION)
    (Infix : Preface_specs.Functor.INFIX) =
struct
  include Core
  include Operation
  include Infix
  module Infix = Infix
end

module Via_map (Req : Preface_specs.Functor.WITH_MAP) = struct
  module Core = Core (Req)
  include Core
  module Operation = Operation (Core)
  include Operation
  module Infix = Infix (Core) (Operation)
  include Infix
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
