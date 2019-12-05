open Preface_core.Fun

module Core_via_functor
    (Functor : Preface_specs.FUNCTOR)
    (Select : Preface_specs.Selective.CORE_VIA_SELECT
                with type 'a t = 'a Functor.t) :
  Preface_specs.Selective.CORE
    with type 'a t = 'a Functor.t
     and module Either = Select.Either = struct
  include Functor
  include Select

  module Ap = struct
    type nonrec 'a t = 'a t

    let pure = pure

    let apply f x = select (map Either.left f) (map ( |> ) x)
  end

  include Applicative.Core_via_apply (Ap)
end

module Core_via_applicative
    (Applicative : Preface_specs.APPLICATIVE)
    (Select : Preface_specs.Selective.CORE_VIA_SELECT
                with type 'a t = 'a Applicative.t) :
  Preface_specs.Selective.CORE
    with type 'a t = 'a Applicative.t
     and module Either = Select.Either = struct
  include Applicative
  include Select
end

module Operation (Core : Preface_specs.Selective.CORE) :
  Preface_specs.Selective.OPERATION
    with type 'a t = 'a Core.t
     and module Either = Core.Either = struct
  include Applicative.Operation (Core)
  module Either = Core.Either

  let branch s l r =
    let open Core in
    let a = map Either.(map_right left) s
    and b = map (compose_right_to_left Either.right) l in
    select (select a b) r

  let if_ predicate if_true unless =
    let open Core in
    branch
      (map (fun b -> Either.(if b then left () else right ())) predicate)
      (map constant if_true)
      (map constant unless)

  let when_ predicate action = if_ predicate action (Core.pure ())

  let rec while_ action = when_ action (while_ action)
end

module Infix
    (Core : Preface_specs.Selective.CORE)
    (Operation : Preface_specs.Selective.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Selective.INFIX
    with type 'a t = 'a Core.t
     and module Either = Core.Either = struct
  include Applicative.Infix (Core) (Operation)
  module Either = Core.Either

  let ( <*? ) = Core.select

  let ( ?*> ) f x = x <*? f

  let ( <||> ) left right = Operation.if_ left (Core.pure true) right

  let ( <&&> ) left right = Operation.if_ left right (Core.pure false)
end

module Syntax (Core : Preface_specs.Selective.CORE) :
  Preface_specs.Selective.SYNTAX with type 'a t = 'a Core.t = struct
  include Applicative.Syntax (Core)
end

module Via
    (Core : Preface_specs.Selective.CORE)
    (Operation : Preface_specs.Selective.OPERATION
                   with type 'a t = 'a Core.t
                    and module Either = Core.Either)
    (Infix : Preface_specs.Selective.INFIX
               with type 'a t = 'a Core.t
                and module Either = Core.Either)
    (Syntax : Preface_specs.Selective.SYNTAX with type 'a t = 'a Core.t) :
  Preface_specs.SELECTIVE with type 'a t = 'a Core.t = struct
  include Core
  include Operation
  include Syntax
  include Infix
  module Infix = Infix
  module Syntax = Syntax
end

module Via_functor
    (Functor : Preface_specs.FUNCTOR)
    (Select : Preface_specs.Selective.CORE_VIA_SELECT
                with type 'a t = 'a Functor.t) :
  Preface_specs.SELECTIVE with type 'a t = 'a Select.t = struct
  module Core = Core_via_functor (Functor) (Select)
  module Operation = Operation (Core)
  module Infix = Infix (Core) (Operation)
  module Syntax = Syntax (Core)
  include Core
  include Operation
  include Infix
  include Syntax
end

module Via_applicative
    (Applicative : Preface_specs.APPLICATIVE)
    (Select : Preface_specs.Selective.CORE_VIA_SELECT
                with type 'a t = 'a Applicative.t) :
  Preface_specs.SELECTIVE with type 'a t = 'a Select.t = struct
  module Core = Core_via_applicative (Applicative) (Select)
  module Operation = Operation (Core)
  module Infix = Infix (Core) (Operation)
  module Syntax = Syntax (Core)
  include Core
  include Operation
  include Infix
  include Syntax
end
