module Core_over_semigroup
    (S : Preface_specs.SEMIGROUP)
    (M : Preface_specs.Monoid.WITH_NEUTRAL with type t = S.t) :
  Preface_specs.Monoid.CORE with type t = M.t = struct
  include S

  let neutral = M.neutral
end

module Core (Req : Preface_specs.Monoid.WITH_NEUTRAL_AND_COMBINE) :
  Preface_specs.Monoid.CORE with type t = Req.t =
  Req

module Operation (Core : Preface_specs.Monoid.CORE) :
  Preface_specs.Monoid.OPERATION with type t = Core.t = struct
  include Semigroup.Operation (Core)

  let reduce list = Preface_core.Monoid.reduce Core.combine Core.neutral list
end

module Infix (Core : Preface_specs.Monoid.CORE) :
  Preface_specs.Monoid.INFIX with type t = Core.t = struct
  include Semigroup.Infix (Core)
end

module Via
    (Core : Preface_specs.Monoid.CORE)
    (Operation : Preface_specs.Monoid.OPERATION with type t = Core.t)
    (Infix : Preface_specs.Monoid.INFIX with type t = Operation.t) :
  Preface_specs.MONOID with type t = Infix.t = struct
  include Core
  include Operation
  module Infix = Infix
  include Infix
end

module Via_combine_and_neutral
    (Req : Preface_specs.Monoid.WITH_NEUTRAL_AND_COMBINE) :
  Preface_specs.MONOID with type t = Req.t = struct
  module Core = Core (Req)
  include Core
  module Operation = Operation (Core)
  module Infix = Infix (Core)
  include Operation
  include Infix
end

module Over_semigroup
    (S : Preface_specs.SEMIGROUP)
    (M : Preface_specs.Monoid.WITH_NEUTRAL with type t = S.t) :
  Preface_specs.MONOID with type t = S.t = struct
  module Core = Core_over_semigroup (S) (M)
  module Operation = Operation (Core)
  module Infix = Infix (Core)
  include Core
  include Operation
  include Infix
end

module From_alternative
    (Alternative : Preface_specs.ALTERNATIVE)
    (T : Preface_specs.Types.T0) :
  Preface_specs.MONOID with type t = T.t Alternative.t =
Via_combine_and_neutral (struct
  type t = T.t Alternative.t

  let combine = Alternative.combine

  let neutral = Alternative.neutral
end)

module From_monad_plus
    (Monad_plus : Preface_specs.MONAD_PLUS)
    (T : Preface_specs.Types.T0) :
  Preface_specs.MONOID with type t = T.t Monad_plus.t =
Via_combine_and_neutral (struct
  type t = T.t Monad_plus.t

  let combine = Monad_plus.combine

  let neutral = Monad_plus.neutral
end)
