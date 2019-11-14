open Fun
open Fun.Infix

module Make_core_via_map_and_duplicate
    (Core : Specs.Comonad.CORE_VIA_MAP_AND_DUPLICATE) :
  Specs.Comonad.CORE with type 'a t = 'a Core.t = struct
  include Core
  include Functor

  let extend f = duplicate %> map f

  let compose_left_to_right f g = duplicate %> map f %> g
end

module Make_core_via_extend (Core : Specs.Comonad.CORE_VIA_EXTEND) :
  Specs.Comonad.CORE with type 'a t = 'a Core.t = struct
  include Core

  let duplicate a = extend id a

  let compose_left_to_right f g = extend f %> g

  let map f = extend @@ (extract %> f)
end

module Make_core_via_cokleisli_composition
    (Core : Specs.Comonad.CORE_VIA_COKLEISLI_COMPOSITION) :
  Specs.Comonad.CORE with type 'a t = 'a Core.t = struct
  include Core

  let extend f = compose_left_to_right f id

  let duplicate a = compose_left_to_right id id a

  let map f = compose_left_to_right (extract %> f) id
end

module Make_syntax (Core : Specs.Comonad.CORE) :
  Specs.Comonad.SYNTAX with type 'a t = 'a Core.t = struct
  type 'a t = 'a Core.t

  let ( let@ ) e f = Core.extend f e
end

module Make_operation (Core : Specs.Comonad.CORE) :
  Specs.Comonad.OPERATION with type 'a t = 'a Core.t = struct
  type 'a t = 'a Core.t

  let lift = Core.map

  let lift2 f a =
    let open Core in
    map @@ extract @@ lift f a

  let lift3 f a b =
    let open Core in
    map @@ extract @@ lift2 f a b

  let compose_right_to_left f g = Core.compose_left_to_right g f

  let rec fix f =
    let open Core in
    extract f (extend fix f)
end

module Make_infix
    (Core : Specs.Comonad.CORE)
    (Operation : Specs.Comonad.OPERATION with type 'a t = 'a Core.t) :
  Specs.Comonad.INFIX with type 'a t = 'a Core.t = struct
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
    (Core : Specs.Comonad.CORE)
    (Operation : Specs.Comonad.OPERATION with type 'a t = 'a Core.t)
    (Syntax : Specs.Comonad.SYNTAX with type 'a t = 'a Core.t)
    (Infix : Specs.Comonad.INFIX with type 'a t = 'a Core.t) :
  Specs.COMONAD with type 'a t = 'a Core.t = struct
  include Core
  include Operation
  include Syntax
  module Syntax = Syntax
  include Infix
  module Infix = Infix
end

module Make_via_map_and_duplicate
    (Core_via_map_and_duplicate : Specs.Comonad.CORE_VIA_MAP_AND_DUPLICATE) :
  Specs.COMONAD with type 'a t = 'a Core_via_map_and_duplicate.t = struct
  module Core = Make_core_via_map_and_duplicate (Core_via_map_and_duplicate)
  module Operation = Make_operation (Core)
  module Syntax = Make_syntax (Core)
  module Infix = Make_infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Make_via_extend (Core_via_extend : Specs.Comonad.CORE_VIA_EXTEND) :
  Specs.COMONAD with type 'a t = 'a Core_via_extend.t = struct
  module Core = Make_core_via_extend (Core_via_extend)
  module Operation = Make_operation (Core)
  module Syntax = Make_syntax (Core)
  module Infix = Make_infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Make_via_cokleisli_composition
    (Core_via_cokleisli_composition : Specs.Comonad
                                      .CORE_VIA_COKLEISLI_COMPOSITION) :
  Specs.COMONAD with type 'a t = 'a Core_via_cokleisli_composition.t = struct
  module Core =
    Make_core_via_cokleisli_composition (Core_via_cokleisli_composition)
  module Operation = Make_operation (Core)
  module Syntax = Make_syntax (Core)
  module Infix = Make_infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end
