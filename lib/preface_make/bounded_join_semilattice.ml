module Core_via_join_and_bottom
    (Req : Preface_specs.Bounded_join_semilattice.WITH_JOIN_AND_BOTTOM) =
  Req

module Core_over_join_semilattice_and_via_bottom
    (Join_req : Preface_specs.Join_semilattice.CORE)
    (Req : Preface_specs.Bounded_join_semilattice.WITH_BOTTOM
             with type t = Join_req.t) =
struct
  include Join_req
  include Req
end

module Infix (Core : Preface_specs.Bounded_join_semilattice.CORE) = struct
  include Join_semilattice.Infix (Core)
end

module Via
    (Core : Preface_specs.Bounded_join_semilattice.CORE)
    (Infix : Preface_specs.Bounded_join_semilattice.INFIX) =
struct
  include Core
  module Infix = Infix
  include Infix
end

module Via_join_and_bottom
    (Req : Preface_specs.Bounded_join_semilattice.WITH_JOIN_AND_BOTTOM) =
struct
  module Core = Core_via_join_and_bottom (Req)
  include Core
  module Infix = Infix (Core)
  include Infix
end

module Over_join_semilattice_and_via_bottom
    (Join_req : Preface_specs.Join_semilattice.CORE)
    (Req : Preface_specs.Bounded_join_semilattice.WITH_BOTTOM
             with type t = Join_req.t) =
struct
  module Core = Core_over_join_semilattice_and_via_bottom (Join_req) (Req)
  include Core
  module Infix = Infix (Core)
  include Infix
end
