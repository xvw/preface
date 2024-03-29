module Core (Req : Preface_specs.Category.WITH_ID_AND_COMPOSE) = Req

module Operation (Core : Preface_specs.Category.CORE) = struct
  include Semigroupoid.Operation (Core)
end

module Infix (Core : Preface_specs.Category.CORE) = struct
  include Semigroupoid.Infix (Core)
end

module Via_id_and_compose (Req : Preface_specs.Category.WITH_ID_AND_COMPOSE) =
struct
  module Core = Core (Req)
  include Core
  include Operation (Core)
  module Infix = Infix (Core)
  include Infix
end

module Via
    (Core : Preface_specs.Category.CORE)
    (Operation : Preface_specs.Category.OPERATION)
    (Infix : Preface_specs.Category.INFIX) =
struct
  include Core
  include Operation
  include Infix
  module Infix = Infix
end

module From_monad (Monad : Preface_specs.Monad.CORE) =
Via_id_and_compose (struct
  type ('a, 'b) t = 'a -> 'b Monad.t

  let id = Monad.return
  let compose f g = Monad.compose_left_to_right g f
end)

module Over_semigroupoid
    (G : Preface_specs.Semigroupoid.WITH_COMPOSE)
    (Req : Preface_specs.Category.WITH_ID with type ('a, 'b) t = ('a, 'b) G.t) =
Via_id_and_compose (struct
  type ('a, 'b) t = ('a, 'b) G.t

  let id = Req.id
  let compose = G.compose
end)

module Product (F : Preface_specs.CATEGORY) (G : Preface_specs.CATEGORY) =
Via_id_and_compose (struct
  type ('a, 'b) t = ('a, 'b) F.t * ('a, 'b) G.t

  let id = (F.id, G.id)
  let compose (x1, y1) (x2, y2) = (F.compose x1 x2, G.compose y1 y2)
end)
