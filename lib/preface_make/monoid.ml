module Core_over_semigroup
    (S : Preface_specs.Semigroup.CORE)
    (M : Preface_specs.Monoid.WITH_NEUTRAL with type t = S.t) =
struct
  include S

  let neutral = M.neutral
end

module Core (Req : Preface_specs.Monoid.WITH_NEUTRAL_AND_COMBINE) = Req

module Operation (Core : Preface_specs.Monoid.CORE) = struct
  include Semigroup.Operation (Core)

  let times n x = Preface_core.Monoid.times Core.combine Core.neutral n x

  let reduce list = Preface_core.Monoid.reduce Core.combine Core.neutral list
end

module Infix (Core : Preface_specs.Monoid.CORE) = struct
  include Semigroup.Infix (Core)
end

module Via
    (Core : Preface_specs.Monoid.CORE)
    (Operation : Preface_specs.Monoid.OPERATION)
    (Infix : Preface_specs.Monoid.INFIX) =
struct
  include Core
  include Operation
  module Infix = Infix
  include Infix
end

module Via_combine_and_neutral
    (Req : Preface_specs.Monoid.WITH_NEUTRAL_AND_COMBINE) =
struct
  module Core = Core (Req)
  include Core
  module Operation = Operation (Core)
  module Infix = Infix (Core)
  include Operation
  include Infix
end

module Over_semigroup
    (S : Preface_specs.SEMIGROUP)
    (M : Preface_specs.Monoid.WITH_NEUTRAL with type t = S.t) =
struct
  module Core = Core_over_semigroup (S) (M)
  module Operation = Operation (Core)
  module Infix = Infix (Core)
  include Core
  include Operation
  include Infix
end

module From_alternative
    (Alternative : Preface_specs.ALTERNATIVE)
    (T : Preface_specs.Types.T0) =
Via_combine_and_neutral (struct
  type t = T.t Alternative.t

  let combine = Alternative.combine

  let neutral = Alternative.neutral
end)

module From_monad_plus
    (Monad_plus : Preface_specs.MONAD_PLUS)
    (T : Preface_specs.Types.T0) =
Via_combine_and_neutral (struct
  type t = T.t Monad_plus.t

  let combine = Monad_plus.combine

  let neutral = Monad_plus.neutral
end)
