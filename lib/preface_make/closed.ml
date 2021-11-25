module Core_via_dimap_and_closed
    (Req : Preface_specs.Closed.WITH_DIMAP_AND_CLOSED) =
struct
  include Req
  include Profunctor.Core_via_dimap (Req)
end

module Core_via_contramap_fst_and_map_snd_and_closed
    (Req : Preface_specs.Closed.WITH_CONTRAMAP_FST_AND_MAP_SND_AND_CLOSED) =
struct
  include Req
  include Profunctor.Core_via_contramap_fst_and_map_snd (Req)
end

module Operation (Core : Preface_specs.Closed.CORE) = struct
  type ('a, 'b) t = ('a, 'b) Core.t

  let curry f = Core.contramap_fst (fun x y -> (x, y)) (Core.closed f)
end

module Via
    (Core : Preface_specs.Closed.CORE)
    (Operation : Preface_specs.Closed.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) =
struct
  include Core
  include Operation
end

module Over_profunctor_via_closed
    (P : Preface_specs.Profunctor.CORE)
    (C : Preface_specs.Closed.WITH_CLOSED with type ('a, 'b) t = ('a, 'b) P.t) =
struct
  module Core = struct
    include P
    include C
  end

  include Core
  include Operation (Core)
end

module Via_dimap_and_closed (Req : Preface_specs.Closed.WITH_DIMAP_AND_CLOSED) =
struct
  module C = struct
    include Profunctor.Via_dimap (Req)
    include Req
  end

  include C
  include Operation (C)
end

module Via_contramap_fst_and_map_snd_and_closed
    (Req : Preface_specs.Closed.WITH_CONTRAMAP_FST_AND_MAP_SND_AND_CLOSED) =
struct
  module C = struct
    include Profunctor.Via_contramap_fst_and_map_snd (Req)
    include Req
  end

  include C
  include Operation (C)
end

module Composition (F : Preface_specs.CLOSED) (G : Preface_specs.CLOSED) =
struct
  module P = Profunctor.Composition (F) (G)

  type ('a, 'b) t = ('a, 'b) P.t =
    | Composed : (('a, 'b) F.t * ('b, 'c) G.t) -> ('a, 'c) t

  include (
    Over_profunctor_via_closed
      (P)
      (struct
        type nonrec ('a, 'b) t = ('a, 'b) t

        let closed (Composed (x, y)) = Composed (F.closed x, G.closed y)
      end) :
        Preface_specs.CLOSED with type ('a, 'b) t := ('a, 'b) t )
end
