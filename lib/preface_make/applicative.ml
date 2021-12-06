open Preface_core.Fun

module Core_via_pure_map_and_product
    (Req : Preface_specs.Applicative.WITH_PURE_MAP_AND_PRODUCT) =
struct
  include Req

  let apply f a = map (fun (f, a) -> f a) @@ product f a
  let lift2 f x y = apply (map f x) y
end

module Core_via_pure_and_apply
    (Req : Preface_specs.Applicative.WITH_PURE_AND_APPLY) =
struct
  include Req

  let map f a = apply (pure f) a
  let product a b = apply (apply (pure (fun a b -> (a, b))) a) b
  let lift2 f x y = apply (map f x) y
end

module Core_via_pure_and_lift2
    (Req : Preface_specs.Applicative.WITH_PURE_AND_LIFT2) =
struct
  include Req

  let apply f a = lift2 (fun x -> x) f a
  let map f a = apply (pure f) a
  let product a b = apply (apply (pure (fun a b -> (a, b))) a) b
end

module Operation (Core : Preface_specs.Applicative.CORE) = struct
  include Functor.Operation (Core)

  let lift = Core.map
  let lift3 f a b = Core.(apply @@ apply (apply (pure f) a) b)
end

module Syntax (Core : Preface_specs.Applicative.CORE) = struct
  type 'a t = 'a Core.t

  let ( let+ ) x f = Core.map f x
  let ( and+ ) = Core.product
end

module Infix
    (Core : Preface_specs.Applicative.CORE)
    (Operation : Preface_specs.Applicative.OPERATION with type 'a t = 'a Core.t) =
struct
  include Functor.Infix (Core) (Operation)

  let ( <*> ) = Core.apply
  let ( <**> ) a b = Core.lift2 (fun x f -> f x) a b
  let ( *> ) a b = Core.lift2 (fun _x y -> y) a b
  let ( <* ) a b = Core.lift2 const a b
end

module Via
    (Core : Preface_specs.Applicative.CORE)
    (Operation : Preface_specs.Applicative.OPERATION)
    (Infix : Preface_specs.Applicative.INFIX)
    (Syntax : Preface_specs.Applicative.SYNTAX) =
struct
  include Core
  include Operation
  include Syntax
  include Infix
  module Infix = Infix
  module Syntax = Syntax
end

module Via_pure_map_and_product
    (Req : Preface_specs.Applicative.WITH_PURE_MAP_AND_PRODUCT) =
struct
  module Core = Core_via_pure_map_and_product (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_pure_and_apply (Req : Preface_specs.Applicative.WITH_PURE_AND_APPLY) =
struct
  module Core = Core_via_pure_and_apply (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_pure_and_lift2 (Req : Preface_specs.Applicative.WITH_PURE_AND_LIFT2) =
struct
  module Core = Core_via_pure_and_lift2 (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module From_monad (Monad : Preface_specs.MONAD) = struct
  include Via_pure_and_apply (struct
    type 'a t = 'a Monad.t

    let pure = Monad.return

    let apply fs xs =
      let open Monad.Syntax in
      let* f = fs in
      let* x = xs in
      pure (f x)
    ;;
  end)
end

module From_alternative (Alternative : Preface_specs.ALTERNATIVE) = Alternative

module Composition
    (F : Preface_specs.APPLICATIVE)
    (G : Preface_specs.APPLICATIVE) =
Via_pure_and_apply (struct
  type 'a t = 'a G.t F.t

  let pure x = F.pure (G.pure x)
  let apply f x = F.lift2 G.apply f x
end)

module From_arrow (A : Preface_specs.ARROW) = Via_pure_and_apply (struct
  type 'a t = (unit, 'a) A.t

  let pure x = A.arrow (const x)
  let uncurry f (x, y) = f x y
  let apply f x = A.(f &&& x >>> arrow (uncurry Fun.id))
end)

module Product (F : Preface_specs.APPLICATIVE) (G : Preface_specs.APPLICATIVE) =
Via_pure_and_apply (struct
  type 'a t = 'a F.t * 'a G.t

  let pure x = (F.pure x, G.pure x)
  let apply (f, g) (x, y) = (F.apply f x, G.apply g y)
end)

module Const (M : Preface_specs.Monoid.CORE) = struct
  type 'a t = Const of M.t

  include (
    Via_pure_and_apply (struct
      type nonrec 'a t = 'a t

      let pure _ = Const M.neutral
      let apply (Const f) (Const x) = Const (M.combine f x)
    end) :
      Preface_specs.APPLICATIVE with type 'a t := 'a t )

  let get (Const value) = value
end
