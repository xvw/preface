module Core_via_divide_and_conquer
    (Divisible_req : Preface_specs.Divisible.WITH_DIVIDE_AND_CONQUER)
    (Req : Preface_specs.Decidable.WITH_LOSE_AND_CHOOSE
             with type 'a t = 'a Divisible_req.t) =
struct
  include Req
  include Divisible.Core_via_divide_and_conquer (Divisible_req)
end

module Core_via_contramap_and_divide_and_conquer
    (Divisible_req : Preface_specs.Divisible
                     .WITH_CONTRAMAP_AND_DIVIDE_AND_CONQUER)
    (Req : Preface_specs.Decidable.WITH_LOSE_AND_CHOOSE
             with type 'a t = 'a Divisible_req.t) =
struct
  include Req
  include Divisible_req
end

module Operation (Core : Preface_specs.Decidable.CORE) = struct
  include Divisible.Operation (Core)

  let lost = Core.lose Fun.id
  let chosen a b = Core.choose Fun.id a b
end

module Infix
    (Core : Preface_specs.Decidable.CORE)
    (Operation : Preface_specs.Decidable.OPERATION with type 'a t = 'a Core.t) =
struct
  include Divisible.Infix (Core) (Operation)

  let ( >|< ) = Operation.chosen
end

module Via
    (Core : Preface_specs.Decidable.CORE)
    (Operation : Preface_specs.Decidable.OPERATION)
    (Infix : Preface_specs.Decidable.INFIX) =
struct
  include Core
  include Operation
  include Infix
  module Infix = Infix
end

module Via_divide_and_conquer
    (Divisible_req : Preface_specs.Divisible.WITH_DIVIDE_AND_CONQUER)
    (Req : Preface_specs.Decidable.WITH_LOSE_AND_CHOOSE
             with type 'a t = 'a Divisible_req.t) =
struct
  module Core = Core_via_divide_and_conquer (Divisible_req) (Req)
  include Core
  module Operation = Operation (Core)
  include Operation
  module Infix = Infix (Core) (Operation)
  include Infix
end

module Via_contramap_and_divide_and_conquer
    (Divisible_req : Preface_specs.Divisible
                     .WITH_CONTRAMAP_AND_DIVIDE_AND_CONQUER)
    (Req : Preface_specs.Decidable.WITH_LOSE_AND_CHOOSE
             with type 'a t = 'a Divisible_req.t) =
struct
  module Core = Core_via_contramap_and_divide_and_conquer (Divisible_req) (Req)
  include Core
  module Operation = Operation (Core)
  include Operation
  module Infix = Infix (Core) (Operation)
  include Infix
end

module Over_divisible
    (Divisible : Preface_specs.Divisible.CORE)
    (Req : Preface_specs.Decidable.WITH_LOSE_AND_CHOOSE
             with type 'a t = 'a Divisible.t) =
  Via_contramap_and_divide_and_conquer (Divisible) (Req)
