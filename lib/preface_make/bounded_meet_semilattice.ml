module Core_via_meet_and_top
    (Req : Preface_specs.Bounded_meet_semilattice.WITH_MEET_AND_TOP) =
  Req

module Core_over_meet_semilattice_and_via_top
    (Meet_req : Preface_specs.Meet_semilattice.CORE)
    (Req : Preface_specs.Bounded_meet_semilattice.WITH_TOP
             with type t = Meet_req.t) =
struct
  include Meet_req
  include Req
end

module Infix (Core : Preface_specs.Bounded_meet_semilattice.CORE) = struct
  include Meet_semilattice.Infix (Core)
end

module Via
    (Core : Preface_specs.Bounded_meet_semilattice.CORE)
    (Infix : Preface_specs.Bounded_meet_semilattice.INFIX) =
struct
  include Core
  module Infix = Infix
  include Infix
end

module Via_meet_and_top
    (Req : Preface_specs.Bounded_meet_semilattice.WITH_MEET_AND_TOP) =
struct
  module Core = Core_via_meet_and_top (Req)
  include Core
  module Infix = Infix (Core)
  include Infix
end

module Over_meet_semilattice_and_via_top
    (Meet_req : Preface_specs.Meet_semilattice.CORE)
    (Req : Preface_specs.Bounded_meet_semilattice.WITH_TOP
             with type t = Meet_req.t) =
struct
  module Core = Core_over_meet_semilattice_and_via_top (Meet_req) (Req)
  include Core
  module Infix = Infix (Core)
  include Infix
end
