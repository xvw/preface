module Core_over_semigroup
    (S : Preface_specs.SEMIGROUP)
    (M : Preface_specs.Monoid.NEUTRAL with type t = S.t) :
  Preface_specs.Monoid.CORE with type t = M.t = struct
  include S

  let neutral = M.neutral
end

module Operation (Core : Preface_specs.Monoid.CORE) :
  Preface_specs.Monoid.OPERATION with type t = Core.t = struct
  include Semigroup.Operation (Core)

  let reduce list = List.fold_left Core.combine Core.neutral list
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

module Via_concat_and_zero (Core : Preface_specs.Monoid.CORE) :
  Preface_specs.MONOID with type t = Core.t = struct
  include Core
  module Operation = Operation (Core)
  module Infix = Infix (Core)
  include Operation
  include Infix
end

module Over_semigroup
    (S : Preface_specs.SEMIGROUP)
    (M : Preface_specs.Monoid.NEUTRAL with type t = S.t) :
  Preface_specs.MONOID with type t = S.t = struct
  module Core = Core_over_semigroup (S) (M)
  module Operation = Operation (Core)
  module Infix = Infix (Core)
  include Core
  include Operation
  include Infix
end
