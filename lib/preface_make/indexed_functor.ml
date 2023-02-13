open Preface_core.Fun
module Core (Req : Preface_specs.Indexed_functor.WITH_MAP) = Req

module Operation (Core : Preface_specs.Indexed_functor.CORE) = struct
  type ('a, 'index) t = ('a, 'index) Core.t

  let replace value x = (Core.map <% const) value x
  let void x = replace () x
end

module Infix
    (Core : Preface_specs.Indexed_functor.CORE)
    (Operation : Preface_specs.Indexed_functor.OPERATION) =
struct
  type ('a, 'index) t = ('a, 'index) Core.t

  let ( <$> ) = Core.map
  let ( <&> ) x f = Core.map f x
  let ( <$ ) value x = Operation.replace value x
  let ( $> ) x value = Operation.replace value x
end

module Syntax (Core : Preface_specs.Indexed_functor.CORE) = struct
  type ('a, 'index) t = ('a, 'index) Core.t

  let ( let+ ) x f = Core.map f x
end

module Via
    (Core : Preface_specs.Indexed_functor.CORE)
    (Operation : Preface_specs.Indexed_functor.OPERATION)
    (Infix : Preface_specs.Indexed_functor.INFIX)
    (Syntax : Preface_specs.Indexed_functor.SYNTAX) =
struct
  include Core
  include Operation
  include Infix
  module Infix = Infix
  include Syntax
  module Syntax = Syntax
end

module Via_map (Req : Preface_specs.Indexed_functor.WITH_MAP) = struct
  module Core = Core (Req)
  include Core
  module Operation = Operation (Core)
  include Operation
  module Infix = Infix (Core) (Operation)
  include Infix
  module Syntax = Syntax (Core)
  include Syntax
end
