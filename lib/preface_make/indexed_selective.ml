open Preface_core.Fun

module Branch_via_select
    (Functor : Preface_specs.Indexed_functor.CORE)
    (Req : Preface_specs.Indexed_selective.WITH_SELECT
             with type ('a, 'index) t = ('a, 'index) Functor.t) =
struct
  let branch s l r =
    let a = Functor.map Either.(map_right left) s
    and b = Functor.map (compose_right_to_left Either.right) l in
    Req.select (Req.select a b) r
  ;;
end

module Select_via_branch
    (Req : Preface_specs.Indexed_selective.WITH_PURE_AND_BRANCH) =
struct
  let select x y = Req.branch x y (Req.pure id)
end

module Core_over_functor_via_select
    (Functor : Preface_specs.Indexed_functor.CORE)
    (Req : Preface_specs.Indexed_selective.WITH_PURE_AND_SELECT
             with type ('a, 'index) t = ('a, 'index) Functor.t) =
struct
  include Functor
  include Req

  module Ap = struct
    type nonrec ('a, 'index) t = ('a, 'index) t

    let pure x = pure x
    let apply f x = select (map Either.left f) (map ( |> ) x)
  end

  include Branch_via_select (Functor) (Req)
  include Indexed_applicative.Core_via_pure_and_apply (Ap)
end

module Core_over_functor_via_branch
    (Functor : Preface_specs.Indexed_functor.CORE)
    (Req : Preface_specs.Indexed_selective.WITH_PURE_AND_BRANCH
             with type ('a, 'index) t = ('a, 'index) Functor.t) =
struct
  include Functor
  include Req
  include Select_via_branch (Req)

  module Ap = struct
    type nonrec ('a, 'index) t = ('a, 'index) t

    let pure x = pure x
    let apply f x = select (map Either.left f) (map ( |> ) x)
  end

  include Indexed_applicative.Core_via_pure_and_apply (Ap)
end

module Core_over_applicative_via_select
    (Applicative : Preface_specs.Indexed_applicative.CORE)
    (Req : Preface_specs.Indexed_selective.WITH_SELECT
             with type ('a, 'index) t = ('a, 'index) Applicative.t) =
struct
  include Applicative
  include Req
  include Branch_via_select (Applicative) (Req)
end

module Core_over_applicative_via_branch
    (Applicative : Preface_specs.Indexed_applicative.CORE)
    (Req : Preface_specs.Indexed_selective.WITH_BRANCH
             with type ('a, 'index) t = ('a, 'index) Applicative.t) =
struct
  include Applicative
  include Req

  include Select_via_branch (struct
    let pure = pure

    include Req
  end)
end

module Operation (Core : Preface_specs.Indexed_selective.CORE) = struct
  include Indexed_applicative.Operation (Core)

  let if_ predicate if_true unless =
    let open Core in
    branch
      (map (fun b -> Either.(if b then left () else right ())) predicate)
      (map const if_true) (map const unless)
  ;;

  let bind_bool x f = if_ x (f false) (f true)
  let when_ predicate action = if_ predicate action (Core.pure ())
  let or_ left right = if_ left (Core.pure true) right
  let and_ left right = if_ left right (Core.pure false)

  let exists predicate =
    let rec aux_exists = function
      | [] -> Core.pure false
      | x :: xs -> if_ (predicate x) (Core.pure true) (aux_exists xs)
    in
    aux_exists
  ;;

  let for_all predicate =
    let rec aux_for_all = function
      | [] -> Core.pure true
      | x :: xs -> if_ (predicate x) (aux_for_all xs) (Core.pure false)
    in
    aux_for_all
  ;;

  let rec while_ action = when_ action (while_ action)
end

module Infix
    (Core : Preface_specs.Indexed_selective.CORE)
    (Operation : Preface_specs.Indexed_selective.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t) =
struct
  include Indexed_applicative.Infix (Core) (Operation)

  let ( <*? ) e f = Core.select e f
  let ( <||> ) l r = Operation.or_ l r
  let ( <&&> ) l r = Operation.and_ l r
end

module Syntax (Core : Preface_specs.Indexed_selective.CORE) =
  Indexed_applicative.Syntax (Core)

module Via
    (Core : Preface_specs.Indexed_selective.CORE)
    (Operation : Preface_specs.Indexed_selective.OPERATION)
    (Infix : Preface_specs.Indexed_selective.INFIX)
    (Syntax : Preface_specs.Indexed_selective.SYNTAX) =
struct
  include Core
  include Operation
  include Syntax
  include Infix
  module Infix = Infix
  module Syntax = Syntax
end

module Over_functor_via_select
    (Functor : Preface_specs.Indexed_functor.CORE)
    (Req : Preface_specs.Indexed_selective.WITH_PURE_AND_SELECT
             with type ('a, 'index) t = ('a, 'index) Functor.t) =
struct
  module Core = Core_over_functor_via_select (Functor) (Req)
  module Operation = Operation (Core)
  module Infix = Infix (Core) (Operation)
  module Syntax = Syntax (Core)
  include Core
  include Operation
  include Infix
  include Syntax
end

module Over_functor_via_branch
    (Functor : Preface_specs.Indexed_functor.CORE)
    (Req : Preface_specs.Indexed_selective.WITH_PURE_AND_BRANCH
             with type ('a, 'index) t = ('a, 'index) Functor.t) =
struct
  module Core = Core_over_functor_via_branch (Functor) (Req)
  module Operation = Operation (Core)
  module Infix = Infix (Core) (Operation)
  module Syntax = Syntax (Core)
  include Core
  include Operation
  include Infix
  include Syntax
end

module Over_applicative_via_select
    (Applicative : Preface_specs.INDEXED_APPLICATIVE)
    (Req : Preface_specs.Indexed_selective.WITH_SELECT
             with type ('a, 'index) t = ('a, 'index) Applicative.t) =
struct
  module Core = Core_over_applicative_via_select (Applicative) (Req)
  module Operation = Operation (Core)
  module Infix = Infix (Core) (Operation)
  module Syntax = Syntax (Core)
  include Core
  include Operation
  include Infix
  include Syntax
end

module Over_applicative_via_branch
    (Applicative : Preface_specs.INDEXED_APPLICATIVE)
    (Req : Preface_specs.Indexed_selective.WITH_BRANCH
             with type ('a, 'index) t = ('a, 'index) Applicative.t) =
struct
  module Core = Core_over_applicative_via_branch (Applicative) (Req)
  module Operation = Operation (Core)
  module Infix = Infix (Core) (Operation)
  module Syntax = Syntax (Core)
  include Core
  include Operation
  include Infix
  include Syntax
end

module Select_from_monad (Monad : Preface_specs.Indexed_monad.CORE) = struct
  type ('a, 'index) t = ('a, 'index) Monad.t

  let pure x = Monad.return x

  let select xs fs =
    Monad.bind
      (fun x ->
        Either.fold ~left:(fun a -> Monad.map (fun f -> f a) fs) ~right:pure x
        )
      xs
  ;;
end
