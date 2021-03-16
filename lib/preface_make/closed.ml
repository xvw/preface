module Core_via_dimap_and_closed
    (Core : Preface_specs.Closed.CORE_WITH_DIMAP_AND_CLOSED) :
  Preface_specs.Closed.CORE with type ('a, 'b) t = ('a, 'b) Core.t = struct
  include Core
  include Profunctor.Via_dimap (Core)
end

module Core_via_contramap_fst_and_map_snd_and_closed
    (Core : Preface_specs.Closed.CORE_WITH_CONTRAMAP_FST_AND_MAP_SND_AND_CLOSED) :
  Preface_specs.Closed.CORE with type ('a, 'b) t = ('a, 'b) Core.t = struct
  include Core
  include Profunctor.Via_contramap_fst_and_map_snd (Core)
end

module Operation (Core : Preface_specs.Closed.CORE) :
  Preface_specs.Closed.OPERATION with type ('a, 'b) t = ('a, 'b) Core.t = struct
  type ('a, 'b) t = ('a, 'b) Core.t

  let curry f = Core.contramap_fst (fun x y -> (x, y)) (Core.closed f)
end

module Via
    (Core : Preface_specs.Closed.CORE)
    (Operation : Preface_specs.Closed.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) :
  Preface_specs.CLOSED with type ('a, 'b) t = ('a, 'b) Operation.t = struct
  include Core
  include Operation
end

module Over_profunctor_via_closed
    (P : Preface_specs.Profunctor.CORE)
    (C : Preface_specs.Closed.WITH_CLOSED with type ('a, 'b) t = ('a, 'b) P.t) :
  Preface_specs.CLOSED with type ('a, 'b) t = ('a, 'b) C.t = struct
  module Core = struct
    include P
    include C
  end

  include Core
  include Operation (Core)
end

module Via_dimap_and_closed
    (Core : Preface_specs.Closed.CORE_WITH_DIMAP_AND_CLOSED) :
  Preface_specs.CLOSED with type ('a, 'b) t = ('a, 'b) Core.t = struct
  module C = struct
    include Profunctor.Via_dimap (Core)
    include Core
  end

  include C
  include Operation (C)
end

module Via_contramap_fst_and_map_snd_and_closed
    (Core : Preface_specs.Closed.CORE_WITH_CONTRAMAP_FST_AND_MAP_SND_AND_CLOSED) :
  Preface_specs.CLOSED with type ('a, 'b) t = ('a, 'b) Core.t = struct
  module C = struct
    include Profunctor.Via_contramap_fst_and_map_snd (Core)
    include Core
  end

  include C
  include Operation (C)
end
