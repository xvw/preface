module Right_via_left
    (D : Preface_specs.Profunctor.WITH_DIMAP)
    (L : Preface_specs.Choice.WITH_LEFT with type ('a, 'b) t = ('a, 'b) D.t) =
struct
  type ('a, 'b) t = ('a, 'b) L.t

  let right x =
    D.dimap Preface_core.Either.swap Preface_core.Either.swap (L.left x)
  ;;
end

module Left_via_right
    (D : Preface_specs.Profunctor.WITH_DIMAP)
    (R : Preface_specs.Choice.WITH_RIGHT with type ('a, 'b) t = ('a, 'b) D.t) =
struct
  type ('a, 'b) t = ('a, 'b) R.t

  let left x =
    D.dimap Preface_core.Either.swap Preface_core.Either.swap (R.right x)
  ;;
end

module Via_dimap_and_left (Core : Preface_specs.Choice.WITH_DIMAP_AND_LEFT) =
struct
  include Core
  module C = Profunctor.Via_dimap (Core)
  include C
  include Right_via_left (C) (Core)
end

module Via_dimap_and_right (Req : Preface_specs.Choice.WITH_DIMAP_AND_RIGHT) =
struct
  include Req
  module C = Profunctor.Via_dimap (Req)
  include C
  include Left_via_right (C) (Req)
end

module Via_contramap_fst_and_map_snd_and_left
    (Req : Preface_specs.Choice.WITH_CONTRAMAP_FST_AND_MAP_SND_AND_LEFT) =
struct
  include Req
  module C = Profunctor.Via_contramap_fst_and_map_snd (Req)
  include C
  include Right_via_left (C) (Req)
end

module Via_contramap_fst_and_map_snd_and_right
    (Req : Preface_specs.Choice.WITH_CONTRAMAP_FST_AND_MAP_SND_AND_RIGHT) =
struct
  include Req
  module C = Profunctor.Via_contramap_fst_and_map_snd (Req)
  include C
  include Left_via_right (C) (Req)
end

module Over_profunctor_via_left
    (P : Preface_specs.Profunctor.CORE)
    (L : Preface_specs.Choice.WITH_LEFT with type ('a, 'b) t = ('a, 'b) P.t) =
struct
  include P
  include L
  include Right_via_left (P) (L)
end

module Over_profunctor_via_right
    (P : Preface_specs.Profunctor.CORE)
    (R : Preface_specs.Choice.WITH_RIGHT with type ('a, 'b) t = ('a, 'b) P.t) =
struct
  include P
  include R
  include Left_via_right (P) (R)
end

module From_applicative (Applicative : Preface_specs.Applicative.CORE) = struct
  module Prof = Profunctor.From_functor (Applicative)

  include
    Over_profunctor_via_left
      (Prof)
      (struct
        type ('a, 'b) t = 'a -> 'b Applicative.t

        let left f = function
          | Either.Right x -> Applicative.pure (Either.Right x)
          | Either.Left x -> Applicative.map Either.left (f x)
        ;;
      end)
end

module Composition (F : Preface_specs.CHOICE) (G : Preface_specs.CHOICE) =
struct
  module P = Profunctor.Composition (F) (G)

  type ('a, 'b) t = ('a, 'b) P.t =
    | Composed : (('a, 'b) F.t * ('b, 'c) G.t) -> ('a, 'c) t

  include (
    Over_profunctor_via_left
      (P)
      (struct
        type nonrec ('a, 'b) t = ('a, 'b) t

        let left (Composed (x, y)) = Composed (F.left x, G.left y)
      end) :
        Preface_specs.CHOICE with type ('a, 'b) t := ('a, 'b) t )
end
