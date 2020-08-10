module Core_via_map_and_product
    (Core : Preface_specs.Alternative.CORE_WITH_MAP_AND_PRODUCT) :
  Preface_specs.Alternative.CORE with type 'a t = 'a Core.t = struct
  include Applicative.Core_via_map_and_product (Core)

  let combine = Core.combine

  let neutral = Core.neutral
end

module Core_via_apply (Core : Preface_specs.Alternative.CORE_WITH_APPLY) :
  Preface_specs.Alternative.CORE with type 'a t = 'a Core.t = struct
  include Applicative.Core_via_apply (Core)

  let combine = Core.combine

  let neutral = Core.neutral
end

module Operation (Core : Preface_specs.Alternative.CORE) :
  Preface_specs.Alternative.OPERATION with type 'a t = 'a Core.t =
  Applicative.Operation (Core)

module Syntax (Core : Preface_specs.Alternative.CORE) :
  Preface_specs.Alternative.SYNTAX with type 'a t = 'a Core.t =
  Applicative.Syntax (Core)

module Infix
    (Core : Preface_specs.Alternative.CORE)
    (Operation : Preface_specs.Alternative.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Alternative.INFIX with type 'a t = 'a Core.t = struct
  include Applicative.Infix (Core) (Operation)

  let ( <|> ) a b = Core.combine a b
end

module Via
    (Core : Preface_specs.Alternative.CORE)
    (Operation : Preface_specs.Alternative.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Alternative.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Alternative.SYNTAX with type 'a t = 'a Core.t) :
  Preface_specs.ALTERNATIVE with type 'a t = 'a Core.t = struct
  include Core
  include Operation
  include Syntax
  include Infix
  module Infix = Infix
  module Syntax = Syntax
end

module Via_map_and_product
    (Core_with_map_and_product : Preface_specs.Alternative
                                 .CORE_WITH_MAP_AND_PRODUCT) :
  Preface_specs.ALTERNATIVE with type 'a t = 'a Core_with_map_and_product.t =
struct
  module Core = Core_via_map_and_product (Core_with_map_and_product)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_apply (Core_with_apply : Preface_specs.Alternative.CORE_WITH_APPLY) :
  Preface_specs.ALTERNATIVE with type 'a t = 'a Core_with_apply.t = struct
  module Core = Core_via_apply (Core_with_apply)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Over_applicative
    (Applicative : Preface_specs.APPLICATIVE)
    (Core : Preface_specs.Alternative.CORE_WITH_NEUTRAL_AND_COMBINE
              with type 'a t = 'a Applicative.t) :
  Preface_specs.ALTERNATIVE with type 'a t = 'a Core.t = struct
  module Core = struct
    include Applicative
    include Core
  end

  include Via_apply (Core)
end
