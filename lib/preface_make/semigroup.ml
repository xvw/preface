module Core (Req : Preface_specs.Semigroup.WITH_COMBINE) :
  Preface_specs.Semigroup.CORE with type t = Req.t =
  Req

module Operation (Core : Preface_specs.Semigroup.CORE) :
  Preface_specs.Semigroup.OPERATION with type t = Core.t = struct
  type t = Core.t

  let times n x = Preface_core.Monoid.times Core.combine n x

  let reduce_nel list = Preface_core.Monoid.reduce_nel Core.combine list
end

module Infix (Core : Preface_specs.Semigroup.CORE) :
  Preface_specs.Semigroup.INFIX with type t = Core.t = struct
  type t = Core.t

  let ( <|> ) = Core.combine
end

module Via
    (Core : Preface_specs.Semigroup.CORE)
    (Operation : Preface_specs.Semigroup.OPERATION with type t = Core.t)
    (Infix : Preface_specs.Semigroup.INFIX with type t = Operation.t) :
  Preface_specs.SEMIGROUP with type t = Infix.t = struct
  include Core
  include Operation
  module Infix = Infix
  include Infix
end

module Via_combine (Req : Preface_specs.Semigroup.WITH_COMBINE) :
  Preface_specs.SEMIGROUP with type t = Req.t = struct
  module Core = Core (Req)
  include Core
  module Operation = Operation (Core)
  module Infix = Infix (Core)
  include Operation
  include Infix
end

module From_alt
    (Alt : Preface_specs.Alt.WITH_COMBINE)
    (T : Preface_specs.Types.T0) :
  Preface_specs.SEMIGROUP with type t = T.t Alt.t = Via_combine (struct
  type t = T.t Alt.t

  let combine = Alt.combine
end)

module From_alternative = From_alt
module From_monad_plus = From_alt
