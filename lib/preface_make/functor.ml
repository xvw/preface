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
