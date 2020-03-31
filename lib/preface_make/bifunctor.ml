open Preface_core.Fun

module Core_via_bimap (Core : Preface_specs.Bifunctor.CORE_WITH_BIMAP) :
  Preface_specs.Bifunctor.CORE with type ('a, 'b) t = ('a, 'b) Core.t = struct
  include Core

  let fst f = bimap f id

  let snd f = bimap id f
end

module Core_via_fst_and_snd
    (Core : Preface_specs.Bifunctor.CORE_WITH_FST_AND_SND) :
  Preface_specs.Bifunctor.CORE with type ('a, 'b) t = ('a, 'b) Core.t = struct
  include Core

  let bimap f g = fst f %> snd g
end

module Operation (Core : Preface_specs.Bifunctor.CORE) :
  Preface_specs.Bifunctor.OPERATION with type ('a, 'b) t = ('a, 'b) Core.t =
struct
  type ('a, 'b) t = ('a, 'b) Core.t

  let replace_fst value x = Core.fst (constant value) x

  let replace_snd value x = Core.snd (constant value) x
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

module Via_fst_and_snd (Core : Preface_specs.Bifunctor.CORE_WITH_FST_AND_SND) :
  Preface_specs.BIFUNCTOR with type ('a, 'b) t = ('a, 'b) Core.t = struct
  module Core = Core_via_fst_and_snd (Core)
  module Operation = Operation (Core)
  include Core
  include Operation
end
