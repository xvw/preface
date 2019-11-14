open Fun

module Core_via_map_and_product
    (Core : Preface_specs.Applicative.CORE_WITH_MAP_AND_PRODUCT) :
  Preface_specs.Applicative.CORE with type 'a t = 'a Core.t = struct
  include Core

  let apply f a = map (fun (f, a) -> f a) @@ product f a
end

module Core_via_apply (Core : Preface_specs.Applicative.CORE_WITH_APPLY) :
  Preface_specs.Applicative.CORE with type 'a t = 'a Core.t = struct
  include Core

  let map f a = apply (pure f) a

  let product a b = apply (apply (pure (fun a b -> a, b)) a) b
end

module Operation (Core : Preface_specs.Applicative.CORE) :
  Preface_specs.Applicative.OPERATION with type 'a t = 'a Core.t = struct
  type 'a t = 'a Core.t

  let lift = Core.map

  let lift2 f a =
    let open Core in
    apply @@ apply (pure f) a

  let lift3 f a b =
    let open Core in
    apply @@ apply (apply (pure f) a) b
end

module Syntax (Core : Preface_specs.Applicative.CORE) :
  Preface_specs.Applicative.SYNTAX with type 'a t = 'a Core.t = struct
  type 'a t = 'a Core.t

  let ( let+ ) x f = Core.map f x

  let ( and+ ) = Core.product
end

module Infix
    (Core : Preface_specs.Applicative.CORE)
    (Operation : Preface_specs.Applicative.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Applicative.INFIX with type 'a t = 'a Core.t = struct
  type 'a t = 'a Core.t

  let ( <*> ) = Core.apply

  let ( <**> ) a f = f <*> a

  let ( *> ) a b = Operation.lift2 const b a

  let ( <* ) a b = b *> a
end

module Via
    (Core : Preface_specs.Applicative.CORE)
    (Operation : Preface_specs.Applicative.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Applicative.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Applicative.SYNTAX with type 'a t = 'a Core.t) :
  Preface_specs.APPLICATIVE with type 'a t = 'a Core.t = struct
  include Core
  include Operation
  include Syntax
  include Infix
  module Infix = Infix
  module Syntax = Syntax
end

module Via_map_and_product
    (Core_with_map_and_product : Preface_specs.Applicative
                                 .CORE_WITH_MAP_AND_PRODUCT) :
  Preface_specs.APPLICATIVE with type 'a t = 'a Core_with_map_and_product.t =
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

module Via_apply (Core_with_apply : Preface_specs.Applicative.CORE_WITH_APPLY) :
  Preface_specs.APPLICATIVE with type 'a t = 'a Core_with_apply.t = struct
  module Core = Core_via_apply (Core_with_apply)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_monad (Monad : Preface_specs.MONAD) :
  Preface_specs.APPLICATIVE with type 'a t = 'a Monad.t = struct
  include Via_apply (struct
    type 'a t = 'a Monad.t

    let pure = Monad.return

    let apply fs xs =
      let open Monad.Syntax in
      let* f = fs in
      let* x = xs in
      pure (f x)
  end)
end