open Preface_core.Fun
open Preface_core.Fun.Infix

module Core_via_map_and_duplicate
    (Core : Preface_specs.Comonad.CORE_WITH_MAP_AND_DUPLICATE) :
  Preface_specs.Comonad.CORE with type 'a t = 'a Core.t = struct
  include Core
  include Functor

  let extend f = duplicate %> map f

  let compose_left_to_right f g = duplicate %> map f %> g
end

module Core_via_extend (Core : Preface_specs.Comonad.CORE_WITH_EXTEND) :
  Preface_specs.Comonad.CORE with type 'a t = 'a Core.t = struct
  include Core

  let duplicate a = extend id a

  let compose_left_to_right f g = extend f %> g

  let map f = extend @@ (extract %> f)
end

module Core_via_cokleisli_composition
    (Core : Preface_specs.Comonad.CORE_WITH_COKLEISLI_COMPOSITION) :
  Preface_specs.Comonad.CORE with type 'a t = 'a Core.t = struct
  include Core

  let extend f = compose_left_to_right f id

  let duplicate a = compose_left_to_right id id a

  let map f = compose_left_to_right (extract %> f) id
end

module Syntax (Core : Preface_specs.Comonad.CORE) :
  Preface_specs.Comonad.SYNTAX with type 'a t = 'a Core.t = struct
  type 'a t = 'a Core.t

  let ( let@ ) e f = Core.extend f e
end

module Operation (Core : Preface_specs.Comonad.CORE) :
  Preface_specs.Comonad.OPERATION with type 'a t = 'a Core.t = struct
  type 'a t = 'a Core.t

  let lift = Core.map

  let lift2 f a =
    let open Core in
    map @@ extract @@ lift f a

  let lift3 f a b =
    let open Core in
    map @@ extract @@ lift2 f a b

  let compose_right_to_left f g = Core.compose_left_to_right g f
end

module Infix
    (Core : Preface_specs.Comonad.CORE)
    (Operation : Preface_specs.Comonad.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Comonad.INFIX with type 'a t = 'a Core.t = struct
  type 'a t = 'a Core.t

  let ( <<= ) = Core.extend

  let ( =>> ) a f = Core.extend f a

  let ( =>= ) = Core.compose_left_to_right

  let ( =<= ) = Operation.compose_right_to_left

  let ( <@> ) f a =
    let open Core in
    map (extract f) a

  let ( <@@> ) a f = f <@> a

  let ( <@ ) a =
    let open Core in
    map @@ extract @@ map constant a

  let ( @> ) a b = b <@ a
end

module Make
    (Core : Preface_specs.Comonad.CORE)
    (Operation : Preface_specs.Comonad.OPERATION with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Comonad.SYNTAX with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Comonad.INFIX with type 'a t = 'a Core.t) :
  Preface_specs.COMONAD with type 'a t = 'a Core.t = struct
  include Core
  include Operation
  include Syntax
  module Syntax = Syntax
  include Infix
  module Infix = Infix
end

module Via_map_and_duplicate
    (Core : Preface_specs.Comonad.CORE_WITH_MAP_AND_DUPLICATE) :
  Preface_specs.COMONAD with type 'a t = 'a Core.t = struct
  module Core = Core_via_map_and_duplicate (Core)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_extend (Core : Preface_specs.Comonad.CORE_WITH_EXTEND) :
  Preface_specs.COMONAD with type 'a t = 'a Core.t = struct
  module Core = Core_via_extend (Core)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_cokleisli_composition
    (Via_cokleisli_composition : Preface_specs.Comonad
                                 .CORE_WITH_COKLEISLI_COMPOSITION) :
  Preface_specs.COMONAD with type 'a t = 'a Via_cokleisli_composition.t = struct
  module Core = Core_via_cokleisli_composition (Via_cokleisli_composition)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end
