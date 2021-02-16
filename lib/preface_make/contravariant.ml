open Preface_core.Fun

module Operation (Core : Preface_specs.Contravariant.CORE) :
  Preface_specs.Contravariant.OPERATION with type 'a t = 'a Core.t = struct
  type 'a t = 'a Core.t

  let replace x c = (Core.contramap % constant) x c
end

module Infix
    (Core : Preface_specs.Contravariant.CORE)
    (Operation : Preface_specs.Contravariant.OPERATION
                   with type 'a t = 'a Core.t) :
  Preface_specs.Contravariant.INFIX with type 'a t = 'a Operation.t = struct
  type 'a t = 'a Core.t

  let ( >$ ) x c = Operation.replace x c

  let ( $< ) c x = Operation.replace x c

  let ( >$< ) f c = Core.contramap f c

  let ( >&< ) c f = Core.contramap f c
end

module Via
    (Core : Preface_specs.Contravariant.CORE)
    (Operation : Preface_specs.Contravariant.OPERATION
                   with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Contravariant.INFIX with type 'a t = 'a Operation.t) :
  Preface_specs.CONTRAVARIANT with type 'a t = 'a Infix.t = struct
  include Core
  include Operation
  include Infix
  module Infix = Infix
end

module Via_contramap (Core : Preface_specs.Contravariant.CORE) :
  Preface_specs.CONTRAVARIANT with type 'a t = 'a Core.t = struct
  include Core
  module Operation = Operation (Core)
  include Operation
  module Infix = Infix (Core) (Operation)
  include Infix
end
