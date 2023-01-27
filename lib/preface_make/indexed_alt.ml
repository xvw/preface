module Core_over_functor
    (Functor : Preface_specs.INDEXED_FUNCTOR)
    (Req : Preface_specs.Indexed_alt.WITH_COMBINE) =
struct
  include Functor
  include Req
end

module Core (Req : Preface_specs.Indexed_alt.WITH_COMBINE_AND_MAP) = Req

module Times_and_reduce_nel (Core : Preface_specs.Indexed_alt.WITH_COMBINE) =
struct
  let times_nel n x = Preface_core.Monoid.times_nel Core.combine n x
  let reduce_nel list = Preface_core.Monoid.reduce_nel Core.combine list
end

module Infix_combine (Core : Preface_specs.Indexed_alt.WITH_COMBINE) = struct
  let ( <|> ) = Core.combine
end

module Operation (Core : Preface_specs.Indexed_alt.CORE) = struct
  include Indexed_functor.Operation (Core)
  include Times_and_reduce_nel (Core)
end

module Infix
    (Core : Preface_specs.Indexed_alt.CORE)
    (Operation : Preface_specs.Indexed_alt.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t) =
struct
  include Indexed_functor.Infix (Core) (Operation)
  include Infix_combine (Core)
end

module Syntax (Core : Preface_specs.Indexed_alt.CORE) = struct
  include Indexed_functor.Syntax (Core)
end

module Via
    (Core : Preface_specs.Indexed_alt.CORE)
    (Operation : Preface_specs.Indexed_alt.OPERATION)
    (Infix : Preface_specs.Indexed_alt.INFIX)
    (Syntax : Preface_specs.Indexed_alt.SYNTAX) =
struct
  include Core
  include Operation
  module Infix = Infix
  include Infix
  module Syntax = Syntax
  include Syntax
end

module Via_map_and_combine
    (Req : Preface_specs.Indexed_alt.WITH_COMBINE_AND_MAP) =
struct
  module Core = Core (Req)
  include Core
  module Operation = Operation (Core)
  module Infix = Infix (Core) (Operation)
  module Syntax = Syntax (Core)
  include Operation
  include Infix
  include Syntax
end

module Over_functor
    (Functor : Preface_specs.INDEXED_FUNCTOR)
    (Combine : Preface_specs.Indexed_alt.WITH_COMBINE) =
struct
  include Functor
  include Combine
  include Times_and_reduce_nel (Combine)

  module Infix = struct
    include Functor.Infix
    include Infix_combine (Combine)
  end

  include Infix
end
