open Preface_core.Fun

module Operation (Core : Preface_specs.Functor.CORE) :
  Preface_specs.Functor.OPERATION with type 'a t = 'a Core.t = struct
  type 'a t = 'a Core.t

  let replace value x = (Core.map <% const) value x

  let void x = replace () x
end

module Infix
    (Core : Preface_specs.Functor.CORE)
    (Operation : Preface_specs.Functor.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Functor.INFIX with type 'a t = 'a Core.t = struct
  type 'a t = 'a Operation.t

  let ( <$> ) = Core.map

  let ( <&> ) x f = Core.map f x

  let ( <$ ) value x = Operation.replace value x

  let ( $> ) x value = Operation.replace value x
end

module Via
    (Core : Preface_specs.Functor.CORE)
    (Operation : Preface_specs.Functor.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Functor.INFIX with type 'a t = 'a Core.t) :
  Preface_specs.FUNCTOR with type 'a t = 'a Core.t = struct
  include Core
  include Operation
  include Infix
  module Infix = Infix
end

module Via_map (Core : Preface_specs.Functor.CORE) :
  Preface_specs.FUNCTOR with type 'a t = 'a Core.t = struct
  include Core
  module Operation = Operation (Core)
  include Operation
  module Infix = Infix (Core) (Operation)
  include Infix
end

module Composition (F : Preface_specs.FUNCTOR) (G : Preface_specs.FUNCTOR) :
  Preface_specs.FUNCTOR with type 'a t = 'a G.t F.t = Via_map (struct
  type 'a t = 'a G.t F.t

  let map f x = F.map (G.map f) x
end)

module From_arrow (A : Preface_specs.ARROW) :
  Preface_specs.FUNCTOR with type 'a t = (unit, 'a) A.t = Via_map (struct
  type 'a t = (unit, 'a) A.t

  let map f x = A.(x >>> arrow f)
end)

module From_applicative (Applicative : Preface_specs.APPLICATIVE) :
  Preface_specs.FUNCTOR with type 'a t = 'a Applicative.t =
  Applicative

module From_alt (Alt : Preface_specs.ALT) :
  Preface_specs.FUNCTOR with type 'a t = 'a Alt.t =
  Alt

module From_monad (Monad : Preface_specs.MONAD) :
  Preface_specs.FUNCTOR with type 'a t = 'a Monad.t =
  Monad

module From_alternative (Alternative : Preface_specs.ALTERNATIVE) :
  Preface_specs.FUNCTOR with type 'a t = 'a Alternative.t =
  Alternative

module From_monad_plus (Monad_plus : Preface_specs.MONAD_PLUS) :
  Preface_specs.FUNCTOR with type 'a t = 'a Monad_plus.t =
  Monad_plus

module From_comonad (Comonad : Preface_specs.COMONAD) :
  Preface_specs.FUNCTOR with type 'a t = 'a Comonad.t =
  Comonad

module Sum (F : Preface_specs.FUNCTOR) (G : Preface_specs.FUNCTOR) = struct
  type 'a sum =
    | L of 'a F.t
    | R of 'a G.t

  include Via_map (struct
    type 'a t = 'a sum

    let map f = function L x -> L (F.map f x) | R x -> R (G.map f x)
  end)
end
