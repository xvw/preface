module Core (Req : Preface_specs.Join_semilattice.WITH_JOIN) = Req

module Infix (Core : Preface_specs.Join_semilattice.CORE) = struct
  type t = Core.t

  let ( || ) = Core.join
end

module Via
    (Core : Preface_specs.Join_semilattice.CORE)
    (Infix : Preface_specs.Join_semilattice.INFIX) =
struct
  include Core
  module Infix = Infix
  include Infix
end

module Via_join (Req : Preface_specs.Join_semilattice.WITH_JOIN) = struct
  module Core = Core (Req)
  include Core
  module Infix = Infix (Core)
  include Infix
end
