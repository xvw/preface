open Fun

module Make_operation (Core : Specs.Functor.CORE) :
  Specs.Functor.OPERATION with type 'a t = 'a Core.t = struct
  type 'a t = 'a Core.t

  let replace value x = (Core.map <% const) value x

  let void x = replace () x
end

module Make_infix
    (Core : Specs.Functor.CORE)
    (Operation : Specs.Functor.OPERATION with type 'a t = 'a Core.t) :
  Specs.Functor.INFIX with type 'a t = 'a Core.t = struct
  type 'a t = 'a Operation.t

  let ( <$> ) = Core.map

  let ( <&> ) x f = Core.map f x

  let ( <$ ) value x = Operation.replace value x

  let ( $> ) x value = Operation.replace value x
end

module Make (Core : Specs.Functor.CORE) :
  Specs.FUNCTOR with type 'a t = 'a Core.t = struct
  include Core
  module Operation = Make_operation (Core)
  include Operation
  module Infix = Make_infix (Core) (Operation)
  include Infix
end
