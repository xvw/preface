module Via_dimap_and_closed
    (Core : Preface_specs.Closed.CORE_WITH_DIMAP_AND_CLOSED) :
  Preface_specs.Closed.CORE with type ('a, 'b) t = ('a, 'b) Core.t = struct
  include Core
  include Profunctor.Via_dimap (Core)
end

module Via_contramap_fst_and_map_snd_and_closed
    (Core : Preface_specs.Closed.CORE_WITH_CONTRAMAP_FST_AND_MAP_SND_AND_CLOSED) :
  Preface_specs.Closed.CORE with type ('a, 'b) t = ('a, 'b) Core.t = struct
  include Core
  include Profunctor.Via_contramap_fst_and_map_snd (Core)
end

module Over_profunctor_via_closed
    (P : Preface_specs.Profunctor.CORE)
    (C : Preface_specs.Closed.WITH_CLOSED with type ('a, 'b) t = ('a, 'b) P.t) :
  Preface_specs.Closed.CORE with type ('a, 'b) t = ('a, 'b) C.t = struct
  include P
  include C
end
