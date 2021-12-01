module Core (Req : Preface_specs.Semigroupoid.WITH_COMPOSE) = Req

module Operation (Core : Preface_specs.Semigroupoid.CORE) = struct
  type ('a, 'b) t = ('a, 'b) Core.t

  let compose_right_to_left f g = Core.compose f g

  let compose_left_to_right f g = Core.compose g f
end

module Infix (Core : Preface_specs.Semigroupoid.CORE) = struct
  type ('a, 'b) t = ('a, 'b) Core.t

  let ( % ) f g = Core.compose f g

  let ( <% ) f g = Core.compose f g

  let ( %> ) f g = Core.compose g f

  let ( <<< ) f g = Core.compose f g

  let ( >>> ) f g = Core.compose g f
end

module Via_compose (Req : Preface_specs.Semigroupoid.WITH_COMPOSE) = struct
  module Core = Core (Req)
  include Core
  include Operation (Core)
  module Infix = Infix (Core)
  include Infix
end

module Via
    (Core : Preface_specs.Semigroupoid.CORE)
    (Operation : Preface_specs.Semigroupoid.OPERATION)
    (Infix : Preface_specs.Semigroupoid.INFIX) =
struct
  include Core
  include Operation
  include Infix
  module Infix = Infix
end

module From_monad (Monad : Preface_specs.Monad.CORE) = Via_compose (struct
  type ('a, 'b) t = 'a -> 'b Monad.t

  let compose f g = Monad.compose_left_to_right g f
end)

module Product (F : Preface_specs.SEMIGROUPOID) (G : Preface_specs.SEMIGROUPOID) =
Via_compose (struct
  type ('a, 'b) t = ('a, 'b) F.t * ('a, 'b) G.t

  let compose (x1, y1) (x2, y2) = (F.compose x1 x2, G.compose y1 y2)
end)
