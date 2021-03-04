open Preface_core.Fun

module Branch_via_select
    (Functor : Preface_specs.Functor.CORE)
    (Select : Preface_specs.Selective.CORE_WITH_SELECT
                with type 'a t = 'a Functor.t) =
struct
  let branch s l r =
    let a = Functor.map Either.(map_right left) s
    and b = Functor.map (compose_right_to_left Either.right) l in
    Select.select (Select.select a b) r
  ;;
end

module Select_via_branch
    (Select : Preface_specs.Selective.CORE_WITH_PURE_AND_BRANCH) =
struct
  let select x y = Select.branch x y (Select.pure id)
end

module Core_over_functor_via_select
    (Functor : Preface_specs.Functor.CORE)
    (Select : Preface_specs.Selective.CORE_WITH_PURE_AND_SELECT
                with type 'a t = 'a Functor.t) :
  Preface_specs.Selective.CORE with type 'a t = 'a Functor.t = struct
  include Functor
  include Select

  module Ap = struct
    type nonrec 'a t = 'a t

    let pure x = pure x

    let apply f x = select (map Either.left f) (map ( |> ) x)
  end

  include Branch_via_select (Functor) (Select)
  include Applicative.Core_via_apply (Ap)
end

module Core_over_functor_via_branch
    (Functor : Preface_specs.Functor.CORE)
    (Branch : Preface_specs.Selective.CORE_WITH_PURE_AND_BRANCH
                with type 'a t = 'a Functor.t) :
  Preface_specs.Selective.CORE with type 'a t = 'a Functor.t = struct
  include Functor
  include Branch
  include Select_via_branch (Branch)

  module Ap = struct
    type nonrec 'a t = 'a t

    let pure x = pure x

    let apply f x = select (map Either.left f) (map ( |> ) x)
  end

  include Applicative.Core_via_apply (Ap)
end

module Core_over_applicative_via_select
    (Applicative : Preface_specs.APPLICATIVE)
    (Select : Preface_specs.Selective.CORE_WITH_SELECT
                with type 'a t = 'a Applicative.t) :
  Preface_specs.Selective.CORE with type 'a t = 'a Applicative.t = struct
  include Applicative
  include Select
  include Branch_via_select (Applicative) (Select)
end

module Core_over_applicative_via_branch
    (Applicative : Preface_specs.APPLICATIVE)
    (Branch : Preface_specs.Selective.CORE_WITH_BRANCH
                with type 'a t = 'a Applicative.t) :
  Preface_specs.Selective.CORE with type 'a t = 'a Applicative.t = struct
  include Applicative
  include Branch

  include Select_via_branch (struct
    let pure = pure

    include Branch
  end)
end

module Operation (Core : Preface_specs.Selective.CORE) :
  Preface_specs.Selective.OPERATION with type 'a t = 'a Core.t = struct
  include Applicative.Operation (Core)

  let if_ predicate if_true unless =
    let open Core in
    branch
      (map (fun b -> Either.(if b then left () else right ())) predicate)
      (map constant if_true) (map constant unless)
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
    (Core : Preface_specs.Selective.CORE)
    (Operation : Preface_specs.Selective.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Selective.INFIX with type 'a t = 'a Core.t = struct
  include Applicative.Infix (Core) (Operation)

  let ( <*? ) e f = Core.select e f

  let ( <||> ) l r = Operation.or_ l r

  let ( <&&> ) l r = Operation.and_ l r
end

module Syntax (Core : Preface_specs.Selective.CORE) :
  Preface_specs.Selective.SYNTAX with type 'a t = 'a Core.t = struct
  include Applicative.Syntax (Core)
end

module Via
    (Core : Preface_specs.Selective.CORE)
    (Operation : Preface_specs.Selective.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Selective.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Selective.SYNTAX with type 'a t = 'a Core.t) :
  Preface_specs.SELECTIVE with type 'a t = 'a Core.t = struct
  include Core
  include Operation
  include Syntax
  include Infix
  module Infix = Infix
  module Syntax = Syntax
end

module Over_functor_via_select
    (Functor : Preface_specs.Functor.CORE)
    (Select : Preface_specs.Selective.CORE_WITH_PURE_AND_SELECT
                with type 'a t = 'a Functor.t) :
  Preface_specs.SELECTIVE with type 'a t = 'a Select.t = struct
  module Core = Core_over_functor_via_select (Functor) (Select)
  module Operation = Operation (Core)
  module Infix = Infix (Core) (Operation)
  module Syntax = Syntax (Core)
  include Core
  include Operation
  include Infix
  include Syntax
end

module Over_functor_via_branch
    (Functor : Preface_specs.Functor.CORE)
    (Branch : Preface_specs.Selective.CORE_WITH_PURE_AND_BRANCH
                with type 'a t = 'a Functor.t) :
  Preface_specs.SELECTIVE with type 'a t = 'a Branch.t = struct
  module Core = Core_over_functor_via_branch (Functor) (Branch)
  module Operation = Operation (Core)
  module Infix = Infix (Core) (Operation)
  module Syntax = Syntax (Core)
  include Core
  include Operation
  include Infix
  include Syntax
end

module Over_applicative_via_select
    (Applicative : Preface_specs.APPLICATIVE)
    (Select : Preface_specs.Selective.CORE_WITH_SELECT
                with type 'a t = 'a Applicative.t) :
  Preface_specs.SELECTIVE with type 'a t = 'a Select.t = struct
  module Core = Core_over_applicative_via_select (Applicative) (Select)
  module Operation = Operation (Core)
  module Infix = Infix (Core) (Operation)
  module Syntax = Syntax (Core)
  include Core
  include Operation
  include Infix
  include Syntax
end

module Over_applicative_via_branch
    (Applicative : Preface_specs.APPLICATIVE)
    (Branch : Preface_specs.Selective.CORE_WITH_BRANCH
                with type 'a t = 'a Applicative.t) :
  Preface_specs.SELECTIVE with type 'a t = 'a Branch.t = struct
  module Core = Core_over_applicative_via_branch (Applicative) (Branch)
  module Operation = Operation (Core)
  module Infix = Infix (Core) (Operation)
  module Syntax = Syntax (Core)
  include Core
  include Operation
  include Infix
  include Syntax
end

module Select_from_monad (Monad : Preface_specs.MONAD) :
  Preface_specs.Selective.CORE_WITH_SELECT with type 'a t = 'a Monad.t = struct
  type 'a t = 'a Monad.t

  let pure x = Monad.return x

  let select xs fs =
    let open Monad.Infix in
    xs >>= Preface_core.Shims.Either.case (fun a -> fs >|= (fun f -> f a)) pure
  ;;
end

module From_arrow_choice (A : Preface_specs.ARROW_CHOICE) :
  Preface_specs.SELECTIVE with type 'a t = (unit, 'a) A.t =
  Over_applicative_via_select
    (Applicative.From_arrow
       (A))
       (struct
         type 'a t = (unit, 'a) A.t

         let to_arrow f =
           A.(arrow (fun x -> ((), x)) >>> fst f >>> arrow (fun (f, x) -> f x))
         ;;

         let select x y = A.(x >>> (to_arrow y ||| return ()))
       end)
