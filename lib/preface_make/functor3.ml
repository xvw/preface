open Preface_core.Fun

module Operation (Core : Preface_specs.Functor3.CORE) = struct
  type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t

  let replace value x = (Core.map <% const) value x

  let void x = replace () x
end

module Infix
    (Core : Preface_specs.Functor3.CORE)
    (Operation : Preface_specs.Functor3.OPERATION
                   with type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t) =
struct
  type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t

  let ( <$> ) = Core.map

  let ( <&> ) x f = Core.map f x

  let ( <$ ) value x = Operation.replace value x

  let ( $> ) x value = Operation.replace value x
end

module Via
    (Core : Preface_specs.Functor3.CORE)
    (Operation : Preface_specs.Functor3.OPERATION
                   with type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t)
    (Infix : Preface_specs.Functor3.INFIX
               with type ('a, 'b, 'c) t = ('a, 'b, 'c) Operation.t) =
struct
  include Core
  include Operation
  include Infix
  module Infix = Infix
end

module Via_map (Core : Preface_specs.Functor3.CORE) = struct
  include Core
  module Operation = Operation (Core)
  include Operation
  module Infix = Infix (Core) (Operation)
  include Infix
end
