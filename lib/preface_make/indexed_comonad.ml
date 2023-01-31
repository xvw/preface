open Preface_core.Fun
open Preface_core.Fun.Infix

module Core_via_map_and_duplicate
    (Req : Preface_specs.Indexed_comonad.WITH_MAP_AND_DUPLICATE) =
struct
  include Req

  let extend f = duplicate %> map f
  let compose_left_to_right f g = duplicate %> map f %> g
end

module Core_via_extend (Req : Preface_specs.Indexed_comonad.WITH_EXTEND) =
struct
  include Req

  let duplicate a = extend id a
  let compose_left_to_right f g = extend f %> g
  let map f = extend @@ (extract %> f)
end

module Core_via_cokleisli_composition
    (Req : Preface_specs.Indexed_comonad.WITH_COKLEISLI_COMPOSITION) =
struct
  include Req

  let extend f = compose_left_to_right f id
  let duplicate a = compose_left_to_right id id a
  let map f = compose_left_to_right (extract %> f) id
end

module Syntax (Core : Preface_specs.Indexed_comonad.CORE) = struct
  type ('a, 'index) t = ('a, 'index) Core.t

  let ( let@ ) e f = Core.extend f e
  let ( let+ ) e f = Core.map f e
end

module Operation (Core : Preface_specs.Indexed_comonad.CORE) = struct
  include Indexed_functor.Operation (Core)

  let lift = Core.map
  let lift2 f a = Core.(map @@ extract @@ lift f a)
  let lift3 f a b = Core.(map @@ extract @@ lift2 f a b)
  let compose_right_to_left f g = Core.compose_left_to_right g f
end

module Infix
    (Core : Preface_specs.Indexed_comonad.CORE)
    (Operation : Preface_specs.Indexed_comonad.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t) =
struct
  include Indexed_functor.Infix (Core) (Operation)

  let ( <<= ) = Core.extend
  let ( =>> ) a f = Core.extend f a
  let ( =>= ) = Core.compose_left_to_right
  let ( =<= ) = Operation.compose_right_to_left
  let ( <@> ) f a = Core.(map (extract f) a)
  let ( <@@> ) a f = f <@> a
  let ( <@ ) a = Core.(map @@ extract @@ map const a)
  let ( @> ) a b = b <@ a
end

module Via
    (Core : Preface_specs.Indexed_comonad.CORE)
    (Operation : Preface_specs.Indexed_comonad.OPERATION)
    (Infix : Preface_specs.Indexed_comonad.INFIX)
    (Syntax : Preface_specs.Indexed_comonad.SYNTAX) =
struct
  include Core
  include Operation
  include Syntax
  module Syntax = Syntax
  include Infix
  module Infix = Infix
end

module Via_map_and_duplicate
    (Req : Preface_specs.Indexed_comonad.WITH_MAP_AND_DUPLICATE) =
struct
  module Core = Core_via_map_and_duplicate (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_extend (Req : Preface_specs.Indexed_comonad.WITH_EXTEND) = struct
  module Core = Core_via_extend (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_cokleisli_composition
    (Req : Preface_specs.Indexed_comonad.WITH_COKLEISLI_COMPOSITION) =
struct
  module Core = Core_via_cokleisli_composition (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end
