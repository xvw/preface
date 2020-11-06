module Operation (Core : Preface_specs.Alt.CORE) :
  Preface_specs.Alt.OPERATION with type 'a t = 'a Core.t = struct
  type 'a t = 'a Core.t

  let times n x =
    if n > 0
    then
      let result = Array.make (pred n) x |> Array.fold_left Core.combine x in
      Some result
    else None
  ;;

  let reduce_nel list = Preface_core.Nonempty_list.reduce Core.combine list
end

module Infix (Core : Preface_specs.Alt.CORE) :
  Preface_specs.Alt.INFIX with type 'a t = 'a Core.t = struct
  type 'a t = 'a Core.t

  let ( <|> ) = Core.combine
end

module Via
    (Core : Preface_specs.Alt.CORE)
    (Operation : Preface_specs.Alt.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Alt.INFIX with type 'a t = 'a Operation.t) :
  Preface_specs.ALT with type 'a t = 'a Infix.t = struct
  include Core
  include Operation
  module Infix = Infix
  include Infix
end

module Via_combine (Core : Preface_specs.Alt.CORE) :
  Preface_specs.ALT with type 'a t = 'a Core.t = struct
  include Core
  module Operation = Operation (Core)
  module Infix = Infix (Core)
  include Operation
  include Infix
end
