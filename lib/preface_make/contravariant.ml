open Preface_core.Fun
module Core (Req : Preface_specs.Contravariant.WITH_CONTRAMAP) = Req

module Operation (Core : Preface_specs.Contravariant.CORE) = struct
  type 'a t = 'a Core.t

  let replace x c = (Core.contramap % const) x c
end

module Infix
    (Core : Preface_specs.Contravariant.CORE)
    (Operation : Preface_specs.Contravariant.OPERATION
                   with type 'a t = 'a Core.t) =
struct
  type 'a t = 'a Core.t

  let ( >$ ) x c = Operation.replace x c

  let ( $< ) c x = Operation.replace x c

  let ( >$< ) f c = Core.contramap f c

  let ( >&< ) c f = Core.contramap f c
end

module Via
    (Core : Preface_specs.Contravariant.CORE)
    (Operation : Preface_specs.Contravariant.OPERATION)
    (Infix : Preface_specs.Contravariant.INFIX) =
struct
  include Core
  include Operation
  include Infix
  module Infix = Infix
end

module Via_contramap (Req : Preface_specs.Contravariant.WITH_CONTRAMAP) = struct
  module Core = Core (Req)
  include Core
  module Operation = Operation (Core)
  include Operation
  module Infix = Infix (Core) (Operation)
  include Infix
end

module Composition (F : Preface_specs.FUNCTOR) (G : Preface_specs.CONTRAVARIANT) =
Via_contramap (struct
  type 'a t = 'a G.t F.t

  let contramap f x = F.map (G.contramap f) x
end)
