module Over (T : Preface_specs.Types.T0) = struct
  type env = T.t

  type 'a t = env -> 'a

  let pure a _ = a

  let map f ma s = f (ma s)

  module Functor = Preface_make.Functor.Via_map (struct
    type nonrec 'a t = 'a t

    let map = map
  end)

  module Applicative = Preface_make.Applicative.Via_apply (struct
    type nonrec 'a t = 'a t

    let pure = pure

    let apply mf ma s = mf s (ma s)
  end)

  module Monad = Preface_make.Monad.Via_bind (struct
    type nonrec 'a t = 'a t

    let return = pure

    let bind f ma s = f (ma s) s
  end)
end
