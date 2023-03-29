module Core_via_join_and_meet (Req : Preface_specs.Lattice.WITH_JOIN_AND_MEET) =
  Req

module Infix (Core : Preface_specs.Lattice.CORE) = struct
  include Join_semilattice.Infix (Core)
  include Meet_semilattice.Infix (Core)
end

module Via
    (Core : Preface_specs.Lattice.CORE)
    (Infix : Preface_specs.Lattice.INFIX) =
struct
  include Core
  module Infix = Infix
  include Infix
end

module Via_join_and_meet (Req : Preface_specs.Lattice.WITH_JOIN_AND_MEET) =
struct
  module Core = Core_via_join_and_meet (Req)
  include Core
  module Infix = Infix (Core)
  include Infix
end

module Over_join_semilattice_and_meet_semilattice
    (Join_req : Preface_specs.Join_semilattice.CORE)
    (Meet_req : Preface_specs.Meet_semilattice.CORE with type t = Join_req.t) =
struct
  include Via_join_and_meet (struct
    include Join_req
    include Meet_req
  end)
end
