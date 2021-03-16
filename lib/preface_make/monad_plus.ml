module Core_via_bind (Core : Preface_specs.Monad_plus.CORE_WITH_BIND) :
  Preface_specs.Monad_plus.CORE with type 'a t = 'a Core.t = struct
  include Monad.Core_via_bind (Core)

  let combine = Core.combine

  let neutral = Core.neutral
end

module Core_via_map_and_join
    (Core : Preface_specs.Monad_plus.CORE_WITH_MAP_AND_JOIN) :
  Preface_specs.Monad_plus.CORE with type 'a t = 'a Core.t = struct
  include Monad.Core_via_map_and_join (Core)

  let combine = Core.combine

  let neutral = Core.neutral
end

module Core_via_kleisli_composition
    (Core : Preface_specs.Monad_plus.CORE_WITH_KLEISLI_COMPOSITION) :
  Preface_specs.Monad_plus.CORE with type 'a t = 'a Core.t = struct
  include Monad.Core_via_kleisli_composition (Core)

  let combine = Core.combine

  let neutral = Core.neutral
end

let filter' bind return neutral predicate m =
  bind (fun x -> if predicate x then return x else neutral) m
;;

module Operation (Core : Preface_specs.Monad_plus.CORE) :
  Preface_specs.Monad_plus.OPERATION with type 'a t = 'a Core.t = struct
  include Monad.Operation (Core)
  include Alt.Operation (Core)

  let reduce list = Preface_core.Monoid.reduce Core.combine Core.neutral list

  let filter predicate m =
    filter' Core.bind Core.return Core.neutral predicate m
  ;;
end

module Operation_over_monad
    (Monad : Preface_specs.MONAD)
    (Core : Preface_specs.Monad_plus.CORE_WITH_NEUTRAL_AND_COMBINE
              with type 'a t = 'a Monad.t) :
  Preface_specs.Monad_plus.OPERATION with type 'a t = 'a Core.t = struct
  include Monad

  include Alt.Operation (struct
    include Monad
    include Core
  end)

  let reduce list = Preface_core.Monoid.reduce Core.combine Core.neutral list

  let filter predicate m =
    filter' Monad.bind Monad.return Core.neutral predicate m
  ;;
end

module Syntax = Monad.Syntax

module Infix
    (Core : Preface_specs.Monad_plus.CORE)
    (Operation : Preface_specs.Monad_plus.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Monad_plus.INFIX with type 'a t = 'a Core.t = struct
  include Monad.Infix (Core) (Operation)

  let ( <|> ) = Core.combine
end

module Via
    (Core : Preface_specs.Monad_plus.CORE)
    (Operation : Preface_specs.Monad_plus.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Monad_plus.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Monad_plus.SYNTAX with type 'a t = 'a Core.t) :
  Preface_specs.MONAD_PLUS with type 'a t = 'a Core.t = struct
  include Core
  include Operation
  include Syntax
  include Infix
  module Syntax = Syntax
  module Infix = Infix
end

module Via_bind (Core_with_bind : Preface_specs.Monad_plus.CORE_WITH_BIND) :
  Preface_specs.MONAD_PLUS with type 'a t = 'a Core_with_bind.t = struct
  module Core = Core_via_bind (Core_with_bind)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_map_and_join
    (Core_with_map_and_join : Preface_specs.Monad_plus.CORE_WITH_MAP_AND_JOIN) :
  Preface_specs.MONAD_PLUS with type 'a t = 'a Core_with_map_and_join.t = struct
  module Core = Core_via_map_and_join (Core_with_map_and_join)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_kleisli_composition
    (Core_with_kleisli_composition : Preface_specs.Monad_plus
                                     .CORE_WITH_KLEISLI_COMPOSITION) :
  Preface_specs.MONAD_PLUS with type 'a t = 'a Core_with_kleisli_composition.t =
struct
  module Core = Core_via_kleisli_composition (Core_with_kleisli_composition)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Over_monad
    (Monad : Preface_specs.MONAD)
    (Core : Preface_specs.Monad_plus.CORE_WITH_NEUTRAL_AND_COMBINE
              with type 'a t = 'a Monad.t) :
  Preface_specs.MONAD_PLUS with type 'a t = 'a Core.t =
  Via
    (struct
      include Monad

      let combine = Core.combine

      let neutral = Core.neutral
    end)
    (Operation_over_monad (Monad) (Core))
    (struct
      type 'a t = 'a Monad.t

      include Monad.Infix

      let ( <|> ) = Core.combine
    end)
    (struct
      type 'a t = 'a Monad.t

      include Monad.Syntax
    end)

module Over_monad_and_alternative
    (Monad : Preface_specs.MONAD)
    (Alternative : Preface_specs.ALTERNATIVE with type 'a t = 'a Monad.t) :
  Preface_specs.MONAD_PLUS with type 'a t = 'a Alternative.t =
  Over_monad (Monad) (Alternative)

module From_arrow_apply_and_arrow_plus
    (A : Preface_specs.ARROW_APPLY)
    (P : Preface_specs.ARROW_PLUS with type ('a, 'b) t = ('a, 'b) A.t) :
  Preface_specs.MONAD_PLUS with type 'a t = (unit, 'a) P.t =
  Over_monad_and_alternative
    (Monad.From_arrow_apply (A)) (Alternative.From_arrow_plus (P))
