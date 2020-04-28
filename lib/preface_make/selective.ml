open Preface_core.Fun

module Core_over_functor_and_either
    (Either : Preface_core.Requirements.EITHER)
    (Functor : Preface_specs.FUNCTOR)
    (Select : Preface_specs.Selective.CORE_WITH_SELECT
                with type 'a t = 'a Functor.t
                 and type ('a, 'b) either = ('a, 'b) Either.t) :
  Preface_specs.Selective.CORE
    with type 'a t = 'a Functor.t
     and type ('a, 'b) either = ('a, 'b) Either.t = struct
  include Functor
  include Select

  module Ap = struct
    type nonrec 'a t = 'a t

    let pure x = pure x

    let apply f x = select (map Either.left f) (map ( |> ) x)
  end

  include Applicative.Core_via_apply (Ap)
end

module Core_over_applicative_and_either
    (Either : Preface_core.Requirements.EITHER)
    (Applicative : Preface_specs.APPLICATIVE)
    (Select : Preface_specs.Selective.CORE_WITH_SELECT
                with type 'a t = 'a Applicative.t
                 and type ('a, 'b) either = ('a, 'b) Either.t) :
  Preface_specs.Selective.CORE
    with type 'a t = 'a Applicative.t
     and type ('a, 'b) either = ('a, 'b) Either.t = struct
  include Applicative
  include Select
end

module Operation_over_either
    (Either : Preface_core.Requirements.EITHER)
    (Core : Preface_specs.Selective.CORE
              with type ('a, 'b) either = ('a, 'b) Either.t) :
  Preface_specs.Selective.OPERATION
    with type 'a t = 'a Core.t
     and type ('a, 'b) either = ('a, 'b) Either.t = struct
  include Applicative.Operation (Core)

  type ('a, 'b) either = ('a, 'b) Either.t

  let branch s l r =
    let open Core in
    let a = map Either.(map_right left) s
    and b = map (compose_right_to_left Either.right) l in
    select (select a b) r
  ;;

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

module Infix_over_either
    (Either : Preface_core.Requirements.EITHER)
    (Core : Preface_specs.Selective.CORE
              with type ('a, 'b) either = ('a, 'b) Either.t)
    (Operation : Preface_specs.Selective.OPERATION
                   with type 'a t = 'a Core.t
                    and type ('a, 'b) either = ('a, 'b) Either.t) :
  Preface_specs.Selective.INFIX
    with type 'a t = 'a Core.t
     and type ('a, 'b) either = ('a, 'b) Either.t = struct
  include Applicative.Infix (Core) (Operation)

  type ('a, 'b) either = ('a, 'b) Either.t

  let ( <*? ) e f = Core.select e f

  let ( <||> ) l r = Operation.or_ l r

  let ( <&&> ) l r = Operation.and_ l r
end

module Syntax_over_either
    (Either : Preface_core.Requirements.EITHER)
    (Core : Preface_specs.Selective.CORE
              with type ('a, 'b) either = ('a, 'b) Either.t) :
  Preface_specs.Selective.SYNTAX with type 'a t = 'a Core.t = struct
  include Applicative.Syntax (Core)
end

module Over_either
    (Either : Preface_core.Requirements.EITHER)
    (Core : Preface_specs.Selective.CORE
              with type ('a, 'b) either = ('a, 'b) Either.t)
    (Operation : Preface_specs.Selective.OPERATION
                   with type 'a t = 'a Core.t
                    and type ('a, 'b) either = ('a, 'b) Either.t)
    (Infix : Preface_specs.Selective.INFIX
               with type 'a t = 'a Core.t
                and type ('a, 'b) either = ('a, 'b) Either.t)
    (Syntax : Preface_specs.Selective.SYNTAX with type 'a t = 'a Core.t) :
  Preface_specs.SELECTIVE
    with type 'a t = 'a Core.t
     and type ('a, 'b) either = ('a, 'b) Either.t = struct
  include Core
  include Operation
  include Syntax
  include Infix
  module Infix = Infix
  module Syntax = Syntax
end

module Over_functor_and_either
    (Either : Preface_core.Requirements.EITHER)
    (Functor : Preface_specs.FUNCTOR)
    (Select : Preface_specs.Selective.CORE_WITH_SELECT
                with type 'a t = 'a Functor.t
                 and type ('a, 'b) either = ('a, 'b) Either.t) :
  Preface_specs.SELECTIVE
    with type 'a t = 'a Select.t
     and type ('a, 'b) either = ('a, 'b) Either.t = struct
  module Core = Core_over_functor_and_either (Either) (Functor) (Select)
  module Operation = Operation_over_either (Either) (Core)
  module Infix = Infix_over_either (Either) (Core) (Operation)
  module Syntax = Syntax_over_either (Either) (Core)
  include Core
  include Operation
  include Infix
  include Syntax
end

module Over_applicative_and_either
    (Either : Preface_core.Requirements.EITHER)
    (Applicative : Preface_specs.APPLICATIVE)
    (Select : Preface_specs.Selective.CORE_WITH_SELECT
                with type 'a t = 'a Applicative.t
                 and type ('a, 'b) either = ('a, 'b) Either.t) :
  Preface_specs.SELECTIVE
    with type 'a t = 'a Select.t
     and type ('a, 'b) either = ('a, 'b) Either.t = struct
  module Core = Core_over_applicative_and_either (Either) (Applicative) (Select)
  module Operation = Operation_over_either (Either) (Core)
  module Infix = Infix_over_either (Either) (Core) (Operation)
  module Syntax = Syntax_over_either (Either) (Core)
  include Core
  include Operation
  include Infix
  include Syntax
end

module Select_from_monad_and_either
    (Either : Preface_core.Requirements.EITHER)
    (Monad : Preface_specs.MONAD) :
  Preface_specs.Selective.CORE_WITH_SELECT
    with type 'a t = 'a Monad.t
     and type ('a, 'b) either = ('a, 'b) Either.t = struct
  type 'a t = 'a Monad.t

  type ('a, 'b) either = ('a, 'b) Either.t

  let pure x = Monad.return x

  let select xs fs =
    Monad.Infix.(xs >>= Either.case (fun a -> fs >|= (fun f -> f a)) pure)
  ;;
end

module Over_applicative = Over_applicative_and_either (Preface_core.Either)
module Over_functor = Over_functor_and_either (Preface_core.Either)
module Core_over_functor = Core_over_functor_and_either (Preface_core.Either)
module Core_over_applicative =
  Core_over_applicative_and_either (Preface_core.Either)
module Operation_over = Operation_over_either (Preface_core.Either)
module Infix_over = Infix_over_either (Preface_core.Either)
module Syntax_over = Syntax_over_either (Preface_core.Either)
module Select_from_monad = Select_from_monad_and_either (Preface_core.Either)
