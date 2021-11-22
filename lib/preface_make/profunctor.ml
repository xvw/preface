module Core_via_dimap (Req : Preface_specs.Profunctor.WITH_DIMAP) :
  Preface_specs.Profunctor.CORE with type ('a, 'b) t = ('a, 'b) Req.t = struct
  include Req

  let contramap_fst f x = Req.dimap f Fun.id x

  let map_snd f x = Req.dimap Fun.id f x
end

module Core_via_contramap_fst_and_map_snd
    (Req : Preface_specs.Profunctor.WITH_CONTRAMAP_FST_AND_MAP_SND) :
  Preface_specs.Profunctor.CORE with type ('a, 'b) t = ('a, 'b) Req.t = struct
  include Req

  let dimap f g x = contramap_fst f (map_snd g x)
end

module Via_dimap (Req : Preface_specs.Profunctor.WITH_DIMAP) :
  Preface_specs.PROFUNCTOR with type ('a, 'b) t = ('a, 'b) Req.t =
  Core_via_dimap (Req)

module Via_contramap_fst_and_map_snd
    (Req : Preface_specs.Profunctor.WITH_CONTRAMAP_FST_AND_MAP_SND) :
  Preface_specs.PROFUNCTOR with type ('a, 'b) t = ('a, 'b) Req.t =
  Core_via_contramap_fst_and_map_snd (Req)

module From_functor (Functor : Preface_specs.Functor.CORE) :
  Preface_specs.PROFUNCTOR with type ('a, 'b) t = 'a -> 'b Functor.t = struct
  type ('a, 'b) t = 'a -> 'b Functor.t

  open Preface_core.Fun.Infix

  let dimap f g h = Functor.map g % h % f

  let contramap_fst k f = f % k

  let map_snd k f = Functor.map k % f
end

module From_strong (Strong : Preface_specs.STRONG) :
  Preface_specs.PROFUNCTOR with type ('a, 'b) t = ('a, 'b) Strong.t =
  Strong

module From_choice (Choice : Preface_specs.CHOICE) :
  Preface_specs.PROFUNCTOR with type ('a, 'b) t = ('a, 'b) Choice.t =
  Choice

module From_closed (Closed : Preface_specs.CLOSED) :
  Preface_specs.PROFUNCTOR with type ('a, 'b) t = ('a, 'b) Closed.t =
  Closed

module Composition (F : Preface_specs.PROFUNCTOR) (G : Preface_specs.PROFUNCTOR) =
struct
  type (_, _) t = Composed : (('a, 'b) F.t * ('b, 'c) G.t) -> ('a, 'c) t

  include (
    Via_dimap (struct
      type nonrec ('a, 'b) t = ('a, 'b) t

      let dimap l r (Composed (f, g)) =
        Composed (F.contramap_fst l f, G.map_snd r g)
      ;;
    end) :
      Preface_specs.PROFUNCTOR with type ('a, 'b) t := ('a, 'b) t )
end
