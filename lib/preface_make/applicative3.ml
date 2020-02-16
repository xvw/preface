open Preface_core.Fun

module Core_via_map_and_product
    (Core : Preface_specs.Applicative3.CORE_WITH_MAP_AND_PRODUCT) =
struct
  include Core

  let apply f a = map (fun (f, a) -> f a) @@ product f a
end

module Core_via_apply (Core : Preface_specs.Applicative3.CORE_WITH_APPLY) =
struct
  include Core

  let map f a = apply (pure f) a

  let product a b = apply (apply (pure (fun a b -> (a, b))) a) b
end

module Operation (Core : Preface_specs.Applicative3.CORE) = struct
  type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t

  let lift = Core.map

  let lift2 f a = Core.(apply @@ apply (pure f) a)

  let lift3 f a b = Core.(apply @@ apply (apply (pure f) a) b)
end

module Syntax (Core : Preface_specs.Applicative3.CORE) = struct
  type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t

  let ( let+ ) x f = Core.map f x

  let ( and+ ) = Core.product
end

module Infix
    (Core : Preface_specs.Applicative3.CORE)
    (Operation : Preface_specs.Applicative3.OPERATION
                   with type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t) =
struct
  type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t

  let ( <$> ) = Core.map

  let ( <*> ) = Core.apply

  let ( <**> ) a f = f <*> a

  let ( *> ) a b = Operation.lift2 const b a

  let ( <* ) a b = b *> a
end

module Via
    (Core : Preface_specs.Applicative3.CORE)
    (Operation : Preface_specs.Applicative3.OPERATION
                   with type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t)
    (Infix : Preface_specs.Applicative3.INFIX
               with type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t)
    (Syntax : Preface_specs.Applicative3.SYNTAX
                with type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t) =
struct
  include Core
  include Operation
  include Syntax
  include Infix
  module Infix = Infix
  module Syntax = Syntax
end

module Via_map_and_product
    (Core_with_map_and_product : Preface_specs.Applicative3
                                 .CORE_WITH_MAP_AND_PRODUCT) =
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

module Via_apply (Core_with_apply : Preface_specs.Applicative3.CORE_WITH_APPLY) =
struct
  module Core = Core_via_apply (Core_with_apply)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

(* module Via_monad (Monad : Preface_specs.MONAD) :
 *   Preface_specs.APPLICATIVE with type 'a t = 'a Monad.t = struct
 *   include Via_apply (struct
 *     type 'a t = 'a Monad.t
 * 
 *     let pure = Monad.return
 * 
 *     let apply fs xs =
 *       let open Monad.Syntax in
 *       let* f = fs in
 *       let* x = xs in
 *       pure (f x)
 *     ;;
 *   end)
 * end *)
