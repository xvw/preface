module Core_via_bind (Req : Preface_specs.Indexed_monad_plus.WITH_BIND) = struct
  include Indexed_monad.Core_via_return_and_bind (Req)

  let combine = Req.combine
  let neutral = Req.neutral
end

module Core_via_map_and_join
    (Req : Preface_specs.Indexed_monad_plus.WITH_MAP_AND_JOIN) =
struct
  include Indexed_monad.Core_via_return_map_and_join (Req)

  let combine = Req.combine
  let neutral = Req.neutral
end

module Core_via_kleisli_composition
    (Req : Preface_specs.Indexed_monad_plus.WITH_KLEISLI_COMPOSITION) =
struct
  include Indexed_monad.Core_via_return_and_kleisli_composition (Req)

  let combine = Req.combine
  let neutral = Req.neutral
end

let filter' bind return neutral predicate m =
  bind (fun x -> if predicate x then return x else neutral) m
;;

module Operation (Core : Preface_specs.Indexed_monad_plus.CORE) = struct
  include Indexed_monad.Operation (Core)
  include Indexed_alt.Operation (Core)

  let times n x = Preface_core.Monoid.times Core.combine Core.neutral n x
  let reduce list = Preface_core.Monoid.reduce Core.combine Core.neutral list

  let filter predicate m =
    filter' Core.bind Core.return Core.neutral predicate m
  ;;
end

module Operation_over_monad
    (Monad : Preface_specs.INDEXED_MONAD)
    (Req : Preface_specs.Indexed_monad_plus.WITH_NEUTRAL_AND_COMBINE
             with type ('a, 'index) t = ('a, 'index) Monad.t) =
struct
  include Monad

  include Indexed_alt.Operation (struct
    include Monad
    include Req
  end)

  let times n x = Preface_core.Monoid.times Req.combine Req.neutral n x
  let reduce list = Preface_core.Monoid.reduce Req.combine Req.neutral list

  let filter predicate m =
    filter' Monad.bind Monad.return Req.neutral predicate m
  ;;
end

module Syntax = Indexed_monad.Syntax

module Infix
    (Core : Preface_specs.Indexed_monad_plus.CORE)
    (Operation : Preface_specs.Indexed_monad_plus.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t) =
struct
  include Indexed_monad.Infix (Core) (Operation)

  let ( <|> ) = Core.combine
end

module Via
    (Core : Preface_specs.Indexed_monad_plus.CORE)
    (Operation : Preface_specs.Indexed_monad_plus.OPERATION)
    (Infix : Preface_specs.Indexed_monad_plus.INFIX)
    (Syntax : Preface_specs.Indexed_monad_plus.SYNTAX) =
struct
  include Core
  include Operation
  include Syntax
  include Infix
  module Syntax = Syntax
  module Infix = Infix
end

module Via_bind (Req : Preface_specs.Indexed_monad_plus.WITH_BIND) = struct
  module Core = Core_via_bind (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_map_and_join
    (Req : Preface_specs.Indexed_monad_plus.WITH_MAP_AND_JOIN) =
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
    (Req : Preface_specs.Indexed_monad_plus.WITH_KLEISLI_COMPOSITION) =
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
    (Monad : Preface_specs.INDEXED_MONAD)
    (Req : Preface_specs.Indexed_monad_plus.WITH_NEUTRAL_AND_COMBINE
             with type ('a, 'index) t = ('a, 'index) Monad.t) =
  Via
    (struct
      include Monad

      let combine = Req.combine
      let neutral = Req.neutral
    end)
    (Operation_over_monad (Monad) (Req))
    (struct
      type ('a, 'index) t = ('a, 'index) Monad.t

      include Monad.Infix

      let ( <|> ) = Req.combine
    end)
    (struct
      type ('a, 'index) t = ('a, 'index) Monad.t

      include Monad.Syntax
    end)

module Over_monad_and_alternative
    (Monad : Preface_specs.INDEXED_MONAD)
    (Alternative : Preface_specs.INDEXED_ALTERNATIVE
                     with type ('a, 'index) t = ('a, 'index) Monad.t) =
  Over_monad (Monad) (Alternative)
