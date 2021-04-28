module Core_over_comonad
    (C : Preface_specs.COMONAD)
    (Env : Preface_specs.Types.T0) =
struct
  type env = Env.t

  type 'a comonad = 'a C.t

  let lower (_, c) = c

  type 'a t = env * 'a comonad

  let run (e, c) = (e, c)

  let ask (e, _) = e

  let asks f (e, _) = f e

  let local f (e, c) = (f e, c)

  module Local (Env : Preface_specs.Types.T0) = struct
    type 'a out = Env.t * 'a comonad

    let run f (e, c) = (f e, c)
  end
end

module Functor (F : Preface_specs.FUNCTOR) (Env : Preface_specs.Types.T0) =
Functor.Via_map (struct
  type 'a t = Env.t * 'a F.t

  let map f (e, c) = (e, F.map f c)
end)

module Comonad (C : Preface_specs.COMONAD) (Env : Preface_specs.Types.T0) =
Comonad.Via_map_and_duplicate (struct
  include Functor (C) (Env)

  let duplicate (e, c) = (e, C.extend (fun x -> (e, x)) c)

  let extract (_, c) = C.extract c
end)

module Applicative (A : Preface_specs.APPLICATIVE) (Env : Preface_specs.MONOID) =
Applicative.Via_apply (struct
  type 'a t = Env.t * 'a A.t

  let pure x = (Env.neutral, A.pure x)

  let apply (m, a) (n, b) = (Env.combine m n, A.apply a b)
end)

module Over_comonad (C : Preface_specs.COMONAD) (Env : Preface_specs.Types.T0) =
struct
  include Core_over_comonad (C) (Env)
  module Comonad = Comonad (C) (Env)
  include Comonad
end
