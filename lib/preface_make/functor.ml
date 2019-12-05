open Preface_core.Fun

module Make_operation (Core : Preface_specs.Functor.CORE) :
  Preface_specs.Functor.OPERATION with type 'a t = 'a Core.t = struct
  type 'a t = 'a Core.t

  let replace value x = (Core.map <% const) value x

  let void x = replace () x
end

module Make_infix
    (Core : Preface_specs.Functor.CORE)
    (Operation : Preface_specs.Functor.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Functor.INFIX with type 'a t = 'a Core.t = struct
  type 'a t = 'a Operation.t

  let ( <$> ) = Core.map

  let ( <&> ) x f = Core.map f x

  let ( <$ ) value x = Operation.replace value x

  let ( $> ) x value = Operation.replace value x
end

module Make
    (Core : Preface_specs.Functor.CORE)
    (Operation : Preface_specs.Functor.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Functor.INFIX with type 'a t = 'a Core.t) :
  Preface_specs.FUNCTOR with type 'a t = 'a Core.t = struct
  include Core
  include Operation
  include Infix
  module Infix = Infix
end

module Make_via_map (Core : Preface_specs.Functor.CORE) :
  Preface_specs.FUNCTOR with type 'a t = 'a Core.t = struct
  include Core
  module Operation = Make_operation (Core)
  include Operation
  module Infix = Make_infix (Core) (Operation)
  include Infix
end
