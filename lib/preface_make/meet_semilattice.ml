module Core (Req : Preface_specs.Meet_semilattice.WITH_MEET) = Req

module Infix (Core : Preface_specs.Meet_semilattice.CORE) = struct
  type t = Core.t

  let ( ^ ) = Core.meet
end

module Via
    (Core : Preface_specs.Meet_semilattice.CORE)
    (Infix : Preface_specs.Meet_semilattice.INFIX) =
struct
  include Core
  module Infix = Infix
  include Infix
end

module Via_meet (Req : Preface_specs.Meet_semilattice.WITH_MEET) = struct
  module Core = Core (Req)
  include Core
  module Infix = Infix (Core)
  include Infix
end
