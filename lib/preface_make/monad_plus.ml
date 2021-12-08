module Core_via_bind (Req : Preface_specs.Monad_plus.WITH_BIND) = struct
  include Monad.Core_via_bind (Req)

  let combine = Req.combine

  let neutral = Req.neutral
end

module Core_via_map_and_join (Req : Preface_specs.Monad_plus.WITH_MAP_AND_JOIN) =
struct
  include Monad.Core_via_map_and_join (Req)

  let combine = Req.combine

  let neutral = Req.neutral
end

module Core_via_kleisli_composition
    (Req : Preface_specs.Monad_plus.WITH_KLEISLI_COMPOSITION) =
struct
  include Monad.Core_via_kleisli_composition (Req)

  let combine = Req.combine

  let neutral = Req.neutral
end

let filter' bind return neutral predicate m =
  bind (fun x -> if predicate x then return x else neutral) m
;;

module Operation (Core : Preface_specs.Monad_plus.CORE) = struct
  include Monad.Operation (Core)
  include Alt.Operation (Core)

  let reduce list = Preface_core.Monoid.reduce Core.combine Core.neutral list

  let filter predicate m =
    filter' Core.bind Core.return Core.neutral predicate m
  ;;
end

module Operation_over_monad
    (Monad : Preface_specs.MONAD)
    (Req : Preface_specs.Monad_plus.WITH_NEUTRAL_AND_COMBINE
             with type 'a t = 'a Monad.t) =
struct
  include Monad

  include Alt.Operation (struct
    include Monad
    include Req
  end)

  let reduce list = Preface_core.Monoid.reduce Req.combine Req.neutral list

  let filter predicate m =
    filter' Monad.bind Monad.return Req.neutral predicate m
  ;;
end

module Syntax = Monad.Syntax

module Infix
    (Core : Preface_specs.Monad_plus.CORE)
    (Operation : Preface_specs.Monad_plus.OPERATION with type 'a t = 'a Core.t) =
struct
  include Monad.Infix (Core) (Operation)

  let ( <|> ) = Core.combine
end

module Via
    (Core : Preface_specs.Monad_plus.CORE)
    (Operation : Preface_specs.Monad_plus.OPERATION)
    (Infix : Preface_specs.Monad_plus.INFIX)
    (Syntax : Preface_specs.Monad_plus.SYNTAX) =
struct
  include Core
  include Operation
  include Syntax
  include Infix
  module Syntax = Syntax
  module Infix = Infix
end

module Via_bind (Req : Preface_specs.Monad_plus.WITH_BIND) = struct
  module Core = Core_via_bind (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_map_and_join (Req : Preface_specs.Monad_plus.WITH_MAP_AND_JOIN) =
struct
  module Core = Core_via_map_and_join (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_kleisli_composition
    (Req : Preface_specs.Monad_plus.WITH_KLEISLI_COMPOSITION) =
struct
  module Core = Core_via_kleisli_composition (Req)
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
    (Req : Preface_specs.Monad_plus.WITH_NEUTRAL_AND_COMBINE
             with type 'a t = 'a Monad.t) =
  Via
    (struct
      include Monad

      let combine = Req.combine

      let neutral = Req.neutral
    end)
    (Operation_over_monad (Monad) (Req))
    (struct
      include Monad.Infix

      let ( <|> ) = Req.combine
    end)
    (Monad.Syntax)

module Over_monad_and_alternative
    (Monad : Preface_specs.MONAD)
    (Alternative : Preface_specs.ALTERNATIVE with type 'a t = 'a Monad.t) =
  Over_monad (Monad) (Alternative)
module From_arrow_apply_and_arrow_plus
    (A : Preface_specs.ARROW_APPLY)
    (P : Preface_specs.ARROW_PLUS with type ('a, 'b) t = ('a, 'b) A.t) =
  Over_monad_and_alternative
    (Monad.From_arrow_apply (A)) (Alternative.From_arrow_plus (P))

module Product (F : Preface_specs.MONAD_PLUS) (G : Preface_specs.MONAD_PLUS) =
  Over_monad
    (Monad.Product (F) (G))
       (struct
         type 'a t = 'a F.t * 'a G.t

         let neutral = (F.neutral, G.neutral)

         let combine (x1, y1) (x2, y2) = (F.combine x1 x2, G.combine y1 y2)
       end)
