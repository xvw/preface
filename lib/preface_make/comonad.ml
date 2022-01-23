open Preface_core.Fun
open Preface_core.Fun.Infix

module Core_via_map_and_duplicate
    (Req : Preface_specs.Comonad.WITH_MAP_AND_DUPLICATE) =
struct
  include Req

  let extend f = duplicate %> map f
  let compose_left_to_right f g = duplicate %> map f %> g
end

module Core_via_extend (Req : Preface_specs.Comonad.WITH_EXTEND) = struct
  include Req

  let duplicate a = extend id a
  let compose_left_to_right f g = extend f %> g
  let map f = extend @@ (extract %> f)
end

module Core_via_cokleisli_composition
    (Req : Preface_specs.Comonad.WITH_COKLEISLI_COMPOSITION) =
struct
  include Req

  let extend f = compose_left_to_right f id
  let duplicate a = compose_left_to_right id id a
  let map f = compose_left_to_right (extract %> f) id
end

module Syntax (Core : Preface_specs.Comonad.CORE) = struct
  type 'a t = 'a Core.t

  let ( let@ ) e f = Core.extend f e
  let ( let+ ) e f = Core.map f e
end

module Operation (Core : Preface_specs.Comonad.CORE) = struct
  include Functor.Operation (Core)

  let lift = Core.map
  let lift2 f a = Core.(map @@ extract @@ lift f a)
  let lift3 f a b = Core.(map @@ extract @@ lift2 f a b)
  let compose_right_to_left f g = Core.compose_left_to_right g f
end

module Infix
    (Core : Preface_specs.Comonad.CORE)
    (Operation : Preface_specs.Comonad.OPERATION with type 'a t = 'a Core.t) =
struct
  include Functor.Infix (Core) (Operation)

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
    (Core : Preface_specs.Comonad.CORE)
    (Operation : Preface_specs.Comonad.OPERATION)
    (Syntax : Preface_specs.Comonad.SYNTAX)
    (Infix : Preface_specs.Comonad.INFIX) =
struct
  include Core
  include Operation
  include Syntax
  module Syntax = Syntax
  include Infix
  module Infix = Infix
end

module Via_map_and_duplicate
    (Req : Preface_specs.Comonad.WITH_MAP_AND_DUPLICATE) =
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

module Via_extend (Req : Preface_specs.Comonad.WITH_EXTEND) = struct
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
    (Req : Preface_specs.Comonad.WITH_COKLEISLI_COMPOSITION) =
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
