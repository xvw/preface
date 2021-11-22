let swap (x, y) = (y, x)

module Snd_via_fst
    (D : Preface_specs.Profunctor.WITH_DIMAP)
    (F : Preface_specs.Strong.WITH_FST with type ('a, 'b) t = ('a, 'b) D.t) :
  Preface_specs.Strong.WITH_SND with type ('a, 'b) t = ('a, 'b) F.t = struct
  type ('a, 'b) t = ('a, 'b) F.t

  let snd x = D.dimap swap swap (F.fst x)
end

module Fst_via_snd
    (D : Preface_specs.Profunctor.WITH_DIMAP)
    (S : Preface_specs.Strong.WITH_SND with type ('a, 'b) t = ('a, 'b) D.t) :
  Preface_specs.Strong.WITH_FST with type ('a, 'b) t = ('a, 'b) S.t = struct
  type ('a, 'b) t = ('a, 'b) S.t

  let fst x = D.dimap swap swap (S.snd x)
end

module Core_via_dimap_and_fst (Req : Preface_specs.Strong.WITH_DIMAP_AND_FST) :
  Preface_specs.Strong.CORE with type ('a, 'b) t = ('a, 'b) Req.t = struct
  include Req
  module C = Profunctor.Core_via_dimap (Req)
  include C
  include Snd_via_fst (C) (Req)
end

module Core_via_dimap_and_snd (Req : Preface_specs.Strong.WITH_DIMAP_AND_SND) :
  Preface_specs.Strong.CORE with type ('a, 'b) t = ('a, 'b) Req.t = struct
  include Req
  module C = Profunctor.Core_via_dimap (Req)
  include C
  include Fst_via_snd (C) (Req)
end

module Core_via_contramap_fst_and_map_snd_and_fst
    (Req : Preface_specs.Strong.WITH_CONTRAMAP_FST_AND_MAP_SND_AND_FST) :
  Preface_specs.Strong.CORE with type ('a, 'b) t = ('a, 'b) Req.t = struct
  include Req
  module C = Profunctor.Core_via_contramap_fst_and_map_snd (Req)
  include C
  include Snd_via_fst (C) (Req)
end

module Core_via_contramap_fst_and_map_snd_and_snd
    (Req : Preface_specs.Strong.WITH_CONTRAMAP_FST_AND_MAP_SND_AND_SND) :
  Preface_specs.Strong.CORE with type ('a, 'b) t = ('a, 'b) Req.t = struct
  include Req
  module C = Profunctor.Core_via_contramap_fst_and_map_snd (Req)
  include C
  include Fst_via_snd (C) (Req)
end

module Core_over_profunctor_via_fst
    (P : Preface_specs.Profunctor.CORE)
    (F : Preface_specs.Strong.WITH_FST with type ('a, 'b) t = ('a, 'b) P.t) :
  Preface_specs.Strong.CORE with type ('a, 'b) t = ('a, 'b) F.t = struct
  include P
  include F
  include Snd_via_fst (P) (F)
end

module Core_over_profunctor_via_snd
    (P : Preface_specs.Profunctor.CORE)
    (S : Preface_specs.Strong.WITH_SND with type ('a, 'b) t = ('a, 'b) P.t) :
  Preface_specs.Strong.CORE with type ('a, 'b) t = ('a, 'b) S.t = struct
  include P
  include S
  include Fst_via_snd (P) (S)
end

module Operation (Core : Preface_specs.Strong.CORE) :
  Preface_specs.Strong.OPERATION with type ('a, 'b) t = ('a, 'b) Core.t = struct
  type ('a, 'b) t = ('a, 'b) Core.t

  let uncurry x = Core.map_snd (fun (f, x) -> f x) (Core.fst x)

  let strong f x =
    Core.dimap (fun x -> (x, x)) (fun (x, y) -> f y x) (Core.fst x)
  ;;
end

module Via
    (Core : Preface_specs.Strong.CORE)
    (Operation : Preface_specs.Strong.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) :
  Preface_specs.STRONG with type ('a, 'b) t = ('a, 'b) Operation.t = struct
  include Core
  include Operation
end

module Via_dimap_and_fst (Req : Preface_specs.Strong.WITH_DIMAP_AND_FST) :
  Preface_specs.STRONG with type ('a, 'b) t = ('a, 'b) Req.t = struct
  module Core = Core_via_dimap_and_fst (Req)
  module Operation = Operation (Core)
  include Core
  include Operation
end

module Via_dimap_and_snd (Req : Preface_specs.Strong.WITH_DIMAP_AND_SND) :
  Preface_specs.STRONG with type ('a, 'b) t = ('a, 'b) Req.t = struct
  module Core = Core_via_dimap_and_snd (Req)
  module Operation = Operation (Core)
  include Core
  include Operation
end

module Via_contramap_fst_and_map_snd_and_fst
    (Req : Preface_specs.Strong.WITH_CONTRAMAP_FST_AND_MAP_SND_AND_FST) :
  Preface_specs.STRONG with type ('a, 'b) t = ('a, 'b) Req.t = struct
  module Core = Core_via_contramap_fst_and_map_snd_and_fst (Req)
  module Operation = Operation (Core)
  include Core
  include Operation
end

module Via_contramap_fst_and_map_snd_and_snd
    (Req : Preface_specs.Strong.WITH_CONTRAMAP_FST_AND_MAP_SND_AND_SND) :
  Preface_specs.STRONG with type ('a, 'b) t = ('a, 'b) Req.t = struct
  module Core = Core_via_contramap_fst_and_map_snd_and_snd (Req)
  module Operation = Operation (Core)
  include Core
  include Operation
end

module Over_profunctor_via_fst
    (P : Preface_specs.PROFUNCTOR)
    (F : Preface_specs.Strong.WITH_FST with type ('a, 'b) t = ('a, 'b) P.t) :
  Preface_specs.STRONG with type ('a, 'b) t = ('a, 'b) F.t = struct
  module Core = Core_over_profunctor_via_fst (P) (F)
  include Core
  include Operation (Core)
end

module Over_profunctor_via_snd
    (P : Preface_specs.PROFUNCTOR)
    (S : Preface_specs.Strong.WITH_SND with type ('a, 'b) t = ('a, 'b) P.t) :
  Preface_specs.STRONG with type ('a, 'b) t = ('a, 'b) S.t = struct
  module Core = Core_over_profunctor_via_snd (P) (S)
  include Core
  include Operation (Core)
end

module From_monad (Monad : Preface_specs.Monad.CORE) :
  Preface_specs.STRONG with type ('a, 'b) t = 'a -> 'b Monad.t = struct
  module Prof = Profunctor.From_functor (Monad)

  include
    Over_profunctor_via_fst
      (Prof)
      (struct
        type ('a, 'b) t = 'a -> 'b Monad.t

        let fst f (a, c) =
          let open Monad in
          bind (fun b -> return (b, c)) (f a)
        ;;
      end)
end

module From_functor (Functor : Preface_specs.Functor.CORE) :
  Preface_specs.STRONG with type ('a, 'b) t = 'a -> 'b Functor.t = struct
  module Prof = Profunctor.From_functor (Functor)

  include
    Over_profunctor_via_fst
      (Prof)
      (struct
        type ('a, 'b) t = 'a -> 'b Functor.t

        let fst f (a, c) = Functor.map (fun x -> (x, c)) (f a)
      end)
end

module Composition (F : Preface_specs.STRONG) (G : Preface_specs.STRONG) =
struct
  module P = Profunctor.Composition (F) (G)

  type ('a, 'b) t = ('a, 'b) P.t =
    | Composed : (('a, 'b) F.t * ('b, 'c) G.t) -> ('a, 'c) t

  include (
    Over_profunctor_via_fst
      (P)
      (struct
        type nonrec ('a, 'b) t = ('a, 'b) t

        let fst (Composed (x, y)) = Composed (F.fst x, G.fst y)
      end) :
        Preface_specs.STRONG with type ('a, 'b) t := ('a, 'b) t )
end
