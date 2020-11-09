module Operation (Core : Preface_specs.Semigroup.CORE) :
  Preface_specs.Semigroup.OPERATION with type t = Core.t = struct
  type t = Core.t

  let times n x =
    if n > 0
    then
      let result = Array.make (pred n) x |> Array.fold_left Core.combine x in
      Some result
    else None
  ;;

  let reduce_nel list = Preface_core.Nonempty_list.reduce Core.combine list
end

module Infix (Core : Preface_specs.Semigroup.CORE) :
  Preface_specs.Semigroup.INFIX with type t = Core.t = struct
  type t = Core.t

  let ( <|> ) = Core.combine
end

module Via
    (Core : Preface_specs.Semigroup.CORE)
    (Operation : Preface_specs.Semigroup.OPERATION with type t = Core.t)
    (Infix : Preface_specs.Semigroup.INFIX with type t = Operation.t) :
  Preface_specs.SEMIGROUP with type t = Infix.t = struct
  include Core
  include Operation
  module Infix = Infix
  include Infix
end

module Via_combine (Core : Preface_specs.Semigroup.CORE) :
  Preface_specs.SEMIGROUP with type t = Core.t = struct
  include Core
  module Operation = Operation (Core)
  module Infix = Infix (Core)
  include Operation
  include Infix
end

module From_alt (Alt : Preface_specs.ALT) (T : Preface_specs.Types.T0) :
  Preface_specs.SEMIGROUP with type t = T.t Alt.t = Via_combine (struct
  type t = T.t Alt.t

  let combine = Alt.combine
end)

module From_alternative = From_alt
module From_monad_plus = From_alt
