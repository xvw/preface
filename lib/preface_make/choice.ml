open Preface_core.Shims

module Right_via_left
    (D : Preface_specs.Profunctor.CORE_WITH_DIMAP)
    (L : Preface_specs.Choice.WITH_LEFT with type ('a, 'b) t = ('a, 'b) D.t) :
  Preface_specs.Choice.WITH_RIGHT with type ('a, 'b) t = ('a, 'b) L.t = struct
  type ('a, 'b) t = ('a, 'b) L.t

  let right x = D.dimap Either.swap Either.swap (L.left x)
end

module Left_via_right
    (D : Preface_specs.Profunctor.CORE_WITH_DIMAP)
    (R : Preface_specs.Choice.WITH_RIGHT with type ('a, 'b) t = ('a, 'b) D.t) :
  Preface_specs.Choice.WITH_LEFT with type ('a, 'b) t = ('a, 'b) R.t = struct
  type ('a, 'b) t = ('a, 'b) R.t

  let left x = D.dimap Either.swap Either.swap (R.right x)
end

module Via_dimap_and_left (Core : Preface_specs.Choice.CORE_WITH_DIMAP_AND_LEFT) :
  Preface_specs.Choice.CORE with type ('a, 'b) t = ('a, 'b) Core.t = struct
  include Core
  module C = Profunctor.Via_dimap (Core)
  include C
  include Right_via_left (C) (Core)
end

module Via_dimap_and_right
    (Core : Preface_specs.Choice.CORE_WITH_DIMAP_AND_RIGHT) :
  Preface_specs.Choice.CORE with type ('a, 'b) t = ('a, 'b) Core.t = struct
  include Core
  module C = Profunctor.Via_dimap (Core)
  include C
  include Left_via_right (C) (Core)
end

module Via_contramap_fst_and_map_snd_and_left
    (Core : Preface_specs.Choice.CORE_WITH_CONTRAMAP_FST_AND_MAP_SND_AND_LEFT) :
  Preface_specs.Choice.CORE with type ('a, 'b) t = ('a, 'b) Core.t = struct
  include Core
  module C = Profunctor.Via_contramap_fst_and_map_snd (Core)
  include C
  include Right_via_left (C) (Core)
end

module Via_contramap_fst_and_map_snd_and_right
    (Core : Preface_specs.Choice.CORE_WITH_CONTRAMAP_FST_AND_MAP_SND_AND_RIGHT) :
  Preface_specs.Choice.CORE with type ('a, 'b) t = ('a, 'b) Core.t = struct
  include Core
  module C = Profunctor.Via_contramap_fst_and_map_snd (Core)
  include C
  include Left_via_right (C) (Core)
end

module Over_profunctor_via_left
    (P : Preface_specs.Profunctor.CORE)
    (L : Preface_specs.Choice.WITH_LEFT with type ('a, 'b) t = ('a, 'b) P.t) :
  Preface_specs.Choice.CORE with type ('a, 'b) t = ('a, 'b) L.t = struct
  include P
  include L
  include Right_via_left (P) (L)
end

module Over_profunctor_via_right
    (P : Preface_specs.Profunctor.CORE)
    (R : Preface_specs.Choice.WITH_RIGHT with type ('a, 'b) t = ('a, 'b) P.t) :
  Preface_specs.Choice.CORE with type ('a, 'b) t = ('a, 'b) R.t = struct
  include P
  include R
  include Left_via_right (P) (R)
end

module From_monad (Monad : Preface_specs.Monad.CORE) :
  Preface_specs.CHOICE with type ('a, 'b) t = 'a -> 'b Monad.t = struct
  module Prof = Profunctor.From_monad (Monad)

  include
    Over_profunctor_via_left
      (Prof)
      (struct
        type ('a, 'b) t = 'a -> 'b Monad.t

        let left f = function
          | Either.Right x -> Monad.return (Either.Right x)
          | Either.Left x -> Monad.map Either.left (f x)
        ;;
      end)
end
