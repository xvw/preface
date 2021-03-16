module Times_and_reduce_nel (Core : Preface_specs.Alt.WITH_COMBINE) = struct
  let times n x = Preface_core.Monoid.times Core.combine n x

  let reduce_nel list = Preface_core.Monoid.reduce_nel Core.combine list
end

module Infix_combine (Core : Preface_specs.Alt.WITH_COMBINE) = struct
  let ( <|> ) = Core.combine
end

module Operation (Core : Preface_specs.Alt.CORE) :
  Preface_specs.Alt.OPERATION with type 'a t = 'a Core.t = struct
  include Functor.Operation (Core)
  include Times_and_reduce_nel (Core)
end

module Infix
    (Core : Preface_specs.Alt.CORE)
    (Operation : Preface_specs.Alt.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Alt.INFIX with type 'a t = 'a Operation.t = struct
  include Functor.Infix (Core) (Operation)
  include Infix_combine (Core)
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

module Via_map_and_combine (Core : Preface_specs.Alt.CORE) :
  Preface_specs.ALT with type 'a t = 'a Core.t = struct
  include Core
  module Operation = Operation (Core)
  module Infix = Infix (Core) (Operation)
  include Operation
  include Infix
end

module Over_functor_via_combine
    (Functor : Preface_specs.FUNCTOR)
    (Combine : Preface_specs.Alt.WITH_COMBINE with type 'a t = 'a Functor.t) :
  Preface_specs.ALT with type 'a t = 'a Combine.t = struct
  include Functor
  include Combine
  include Times_and_reduce_nel (Combine)

  module Infix = struct
    include Functor.Infix
    include Infix_combine (Combine)
  end

  include Infix
end
