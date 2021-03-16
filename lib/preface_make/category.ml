module Operation (Core : Preface_specs.Category.CORE) :
  Preface_specs.Category.OPERATION with type ('a, 'b) t = ('a, 'b) Core.t =
struct
  type ('a, 'b) t = ('a, 'b) Core.t

  let compose_right_to_left f g = Core.compose f g

  let compose_left_to_right f g = Core.compose g f
end

module Infix (Core : Preface_specs.Category.CORE) :
  Preface_specs.Category.INFIX with type ('a, 'b) t = ('a, 'b) Core.t = struct
  type ('a, 'b) t = ('a, 'b) Core.t

  let ( % ) f g = Core.compose f g

  let ( <% ) f g = Core.compose f g

  let ( %> ) f g = Core.compose g f

  let ( <<< ) f g = Core.compose f g

  let ( >>> ) f g = Core.compose g f
end

module Via_id_and_compose (Core : Preface_specs.Category.CORE) :
  Preface_specs.CATEGORY with type ('a, 'b) t = ('a, 'b) Core.t = struct
  include Core
  include Operation (Core)
  module Infix = Infix (Core)
  include Infix
end

module Via
    (Core : Preface_specs.Category.CORE)
    (Operation : Preface_specs.Category.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t)
    (Infix : Preface_specs.Category.INFIX
               with type ('a, 'b) t = ('a, 'b) Operation.t) :
  Preface_specs.CATEGORY with type ('a, 'b) t = ('a, 'b) Infix.t = struct
  include Core
  include Operation
  include Infix
  module Infix = Infix
end

module From_monad (Monad : Preface_specs.Monad.CORE) :
  Preface_specs.CATEGORY with type ('a, 'b) t = 'a -> 'b Monad.t =
Via_id_and_compose (struct
  type ('a, 'b) t = 'a -> 'b Monad.t

  let id = Monad.return

  let compose f g = Monad.compose_left_to_right g f
end)

module Product (F : Preface_specs.CATEGORY) (G : Preface_specs.CATEGORY) :
  Preface_specs.CATEGORY with type ('a, 'b) t = ('a, 'b) F.t * ('a, 'b) G.t =
Via_id_and_compose (struct
  type ('a, 'b) t = ('a, 'b) F.t * ('a, 'b) G.t

  let id = (F.id, G.id)

  let compose (x1, y1) (x2, y2) = (F.compose x1 x2, G.compose y1 y2)
end)
