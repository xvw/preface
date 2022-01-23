module Core_over_comonad
    (C : Preface_specs.COMONAD)
    (Store : Preface_specs.Types.T0) =
struct
  type store = Store.t
  type 'a comonad = 'a C.t

  let lower (f, s) = C.map (fun g -> g s) f

  type 'a t = (store -> 'a) comonad * store

  let run (f, s) = (f, s)
  let pos (_, s) = s
  let peek s (g, _) = (C.extract g) s
  let peeks f (g, s) = (C.extract g) (f s)
  let seek s (f, _) = (f, s)
  let seeks f (g, s) = (g, f s)

  module Experiment (F : Preface_specs.FUNCTOR) = struct
    let run f (g, s) = F.map (C.extract g) (f s)
  end
end

module Comonad (C : Preface_specs.COMONAD) (Store : Preface_specs.Types.T0) =
Comonad.Via_extend (struct
  type 'a t = (Store.t -> 'a) C.t * Store.t

  let extract (g, s) = (C.extract g) s
  let extend f (g, s) = (C.extend (fun g' s' -> f (g', s')) g, s)
end)

module Functor (F : Preface_specs.FUNCTOR) (Store : Preface_specs.Types.T0) =
Functor.Via_map (struct
  type 'a t = (Store.t -> 'a) F.t * Store.t

  let map f (g, s) = (F.map (fun k x -> f (k x)) g, s)
end)

module Over_comonad (C : Preface_specs.COMONAD) (Store : Preface_specs.Types.T0) =
struct
  include Core_over_comonad (C) (Store)
  module Comonad = Comonad (C) (Store)
  include Comonad
end
