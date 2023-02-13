module Core_via_pure_map_and_product
    (Req : Preface_specs.Indexed_alternative.WITH_PURE_MAP_AND_PRODUCT) =
struct
  include Indexed_applicative.Core_via_pure_map_and_product (Req)

  let combine = Req.combine
  let neutral = Req.neutral
end

module Core_via_pure_and_apply
    (Req : Preface_specs.Indexed_alternative.WITH_PURE_AND_APPLY) =
struct
  include Indexed_applicative.Core_via_pure_and_apply (Req)

  let combine = Req.combine
  let neutral = Req.neutral
end

module Core_via_pure_and_lift2
    (Req : Preface_specs.Indexed_alternative.WITH_PURE_AND_LIFT2) =
struct
  include Indexed_applicative.Core_via_pure_and_lift2 (Req)

  let combine = Req.combine
  let neutral = Req.neutral
end

module Operation (Core : Preface_specs.Indexed_alternative.CORE) = struct
  include Indexed_applicative.Operation (Core)
  include Indexed_alt.Operation (Core)

  let times n x = Preface_core.Monoid.times Core.combine Core.neutral n x
  let reduce list = List.fold_left Core.combine Core.neutral list
end

module Syntax (Core : Preface_specs.Indexed_alternative.CORE) =
  Indexed_applicative.Syntax (Core)

module Infix
    (Core : Preface_specs.Indexed_alternative.CORE)
    (Operation : Preface_specs.Indexed_alternative.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t) =
struct
  include Indexed_applicative.Infix (Core) (Operation)
  include Indexed_alt.Infix (Core) (Operation)
end

module Via
    (Core : Preface_specs.Indexed_alternative.CORE)
    (Operation : Preface_specs.Indexed_alternative.OPERATION)
    (Infix : Preface_specs.Indexed_alternative.INFIX)
    (Syntax : Preface_specs.Indexed_alternative.SYNTAX) =
struct
  include Core
  include Operation
  include Syntax
  include Infix
  module Infix = Infix
  module Syntax = Syntax
end

module Via_pure_map_and_product
    (Req : Preface_specs.Indexed_alternative.WITH_PURE_MAP_AND_PRODUCT) =
struct
  module Core = Core_via_pure_map_and_product (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_pure_and_apply
    (Req : Preface_specs.Indexed_alternative.WITH_PURE_AND_APPLY) =
struct
  module Core = Core_via_pure_and_apply (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_pure_and_lift2
    (Req : Preface_specs.Indexed_alternative.WITH_PURE_AND_LIFT2) =
struct
  module Core = Core_via_pure_and_lift2 (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Over_applicative
    (Applicative : Preface_specs.INDEXED_APPLICATIVE)
    (Req : Preface_specs.Indexed_alternative.WITH_NEUTRAL_AND_COMBINE
             with type ('a, 'index) t = ('a, 'index) Applicative.t) =
  Via
    (struct
      include Applicative

      let combine = Req.combine
      let neutral = Req.neutral
    end)
    (struct
      include Indexed_alt.Operation (struct
        include Applicative
        include Req
      end)

      include Applicative

      let times n x = Preface_core.Monoid.times Req.combine Req.neutral n x
      let reduce list = List.fold_left Req.combine Req.neutral list
    end)
    (struct
      type ('a, 'index) t = ('a, 'index) Applicative.t

      include Applicative.Infix

      let ( <|> ) = Req.combine
    end)
    (struct
      type ('a, 'index) t = ('a, 'index) Applicative.t

      include Applicative.Syntax
    end)
