open Preface_core.Fun

module Core_via_bimap (Req : Preface_specs.Bifunctor.WITH_BIMAP) = struct
  include Req

  let map_fst f = bimap f id
  let map_snd f = bimap id f
end

module Core_via_map_fst_and_map_snd
    (Req : Preface_specs.Bifunctor.WITH_MAP_FST_AND_MAP_SND) =
struct
  include Req

  let bimap f g = map_fst f %> map_snd g
end

module Operation (Core : Preface_specs.Bifunctor.CORE) = struct
  type ('a, 'b) t = ('a, 'b) Core.t

  let replace_fst value x = Core.map_fst (const value) x
  let replace_snd value x = Core.map_snd (const value) x
end

module Via
    (Core : Preface_specs.Bifunctor.CORE)
    (Operation : Preface_specs.Bifunctor.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) =
struct
  include Core
  include Operation
end

module Via_bimap (Req : Preface_specs.Bifunctor.WITH_BIMAP) = struct
  module Core = Core_via_bimap (Req)
  module Operation = Operation (Core)
  include Core
  include Operation
end

module Via_map_fst_and_map_snd
    (Req : Preface_specs.Bifunctor.WITH_MAP_FST_AND_MAP_SND) =
struct
  module Core = Core_via_map_fst_and_map_snd (Req)
  module Operation = Operation (Core)
  include Core
  include Operation
end

module From_functors_product
    (F : Preface_specs.FUNCTOR)
    (G : Preface_specs.FUNCTOR) =
Via_map_fst_and_map_snd (struct
  type ('a, 'b) t = 'a F.t * 'b G.t

  let map_fst f (x, y) = (F.map f x, y)
  let map_snd f (x, y) = (x, G.map f y)
end)

module From_functors_sum (F : Preface_specs.FUNCTOR) (G : Preface_specs.FUNCTOR) =
struct
  type ('a, 'b) sum =
    | L of 'a F.t
    | R of 'b G.t

  include Via_bimap (struct
    type ('a, 'b) t = ('a, 'b) sum

    let bimap f g = function L x -> L (F.map f x) | R x -> R (G.map g x)
  end)
end

module Product (F : Preface_specs.BIFUNCTOR) (G : Preface_specs.BIFUNCTOR) =
Via_bimap (struct
  type ('a, 'b) t = ('a, 'b) F.t * ('a, 'b) G.t

  let bimap f g (x, y) = (F.bimap f g x, G.bimap f g y)
end)

module Sum (F : Preface_specs.BIFUNCTOR) (G : Preface_specs.BIFUNCTOR) = struct
  type ('a, 'b) sum =
    | L of ('a, 'b) F.t
    | R of ('a, 'b) G.t

  include Via_bimap (struct
    type ('a, 'b) t = ('a, 'b) sum

    let bimap f g = function
      | L x -> L (F.bimap f g x)
      | R x -> R (G.bimap f g x)
    ;;
  end)
end

module From_functor (F : Preface_specs.Functor.CORE) = Via_bimap (struct
  type ('a, 'b) t = 'a F.t

  let bimap f _ x = F.map f x
end)
