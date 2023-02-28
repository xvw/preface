module Core_via_bounded_join_lattice_and_bounded_meet_lattice
    (Req : Preface_specs.Bounded_lattice
           .WITH_BOUNDED_JOIN_LATTICE_AND_BOUNDED_MEET_LATTICE) =
  Req

module Core_over_bounded_join_lattice_and_bounded_meet_lattice
    (Join_req : Preface_specs.Bounded_join_semilattice.CORE)
    (Meet_req : Preface_specs.Bounded_meet_semilattice.CORE
                  with type t = Join_req.t) =
struct
  include Join_req
  include Meet_req
end

module Infix (Core : Preface_specs.Bounded_lattice.CORE) = struct
  include Bounded_join_semilattice.Infix (Core)
  include Bounded_meet_semilattice.Infix (Core)
end

module Via
    (Core : Preface_specs.Bounded_lattice.CORE)
    (Infix : Preface_specs.Bounded_lattice.INFIX) =
struct
  include Core
  module Infix = Infix
  include Infix
end

module Via_bounded_join_lattice_and_bounded_meet_lattice
    (Req : Preface_specs.Bounded_lattice
           .WITH_BOUNDED_JOIN_LATTICE_AND_BOUNDED_MEET_LATTICE) =
struct
  module Core = Core_via_bounded_join_lattice_and_bounded_meet_lattice (Req)
  include Core
  module Infix = Infix (Core)
  include Infix
end

module Over_bounded_join_lattice_and_bounded_meet_lattice
    (Join_req : Preface_specs.Bounded_join_semilattice.CORE)
    (Meet_req : Preface_specs.Bounded_meet_semilattice.CORE
                  with type t = Join_req.t) =
struct
  module Core =
    Core_over_bounded_join_lattice_and_bounded_meet_lattice
      (Join_req)
      (Meet_req)

  include Core
  module Infix = Infix (Core)
  include Infix
end
