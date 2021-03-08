open Preface_core.Fun

module Core_via_bimap (Core : Preface_specs.Bifunctor.CORE_WITH_BIMAP) :
  Preface_specs.Bifunctor.CORE with type ('a, 'b) t = ('a, 'b) Core.t = struct
  include Core

  let map_fst f = bimap f id

  let map_snd f = bimap id f
end

module Core_via_map_fst_and_map_snd
    (Core : Preface_specs.Bifunctor.CORE_WITH_MAP_FST_AND_MAP_SND) :
  Preface_specs.Bifunctor.CORE with type ('a, 'b) t = ('a, 'b) Core.t = struct
  include Core

  let bimap f g = map_fst f %> map_snd g
end

module Operation (Core : Preface_specs.Bifunctor.CORE) :
  Preface_specs.Bifunctor.OPERATION with type ('a, 'b) t = ('a, 'b) Core.t =
struct
  type ('a, 'b) t = ('a, 'b) Core.t

  let replace_fst value x = Core.map_fst (constant value) x

  let replace_snd value x = Core.map_snd (constant value) x
end

module Via
    (Core : Preface_specs.Bifunctor.CORE)
    (Operation : Preface_specs.Bifunctor.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) :
  Preface_specs.BIFUNCTOR with type ('a, 'b) t = ('a, 'b) Core.t = struct
  include Core
  include Operation
end

module Via_bimap (Core : Preface_specs.Bifunctor.CORE_WITH_BIMAP) :
  Preface_specs.BIFUNCTOR with type ('a, 'b) t = ('a, 'b) Core.t = struct
  module Core = Core_via_bimap (Core)
  module Operation = Operation (Core)
  include Core
  include Operation
end

module Via_map_fst_and_map_snd
    (Core : Preface_specs.Bifunctor.CORE_WITH_MAP_FST_AND_MAP_SND) :
  Preface_specs.BIFUNCTOR with type ('a, 'b) t = ('a, 'b) Core.t = struct
  module Core = Core_via_map_fst_and_map_snd (Core)
  module Operation = Operation (Core)
  include Core
  include Operation
end
