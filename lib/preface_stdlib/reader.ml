module Over (T : Preface_specs.Types.T0) = struct
  open Preface_core.Fun

  type env = T.t

  type 'a t = env -> 'a

  let run = id

  let pure = constant

  let map = ( <% )

  module Functor = Preface_make.Functor.Via_map (struct
    type nonrec 'a t = 'a t

    let map = map
  end)

  module Applicative = Preface_make.Applicative.Via_apply (struct
    type nonrec 'a t = 'a t

    let pure = pure

    let apply mf ma s = map (mf s) ma s
    (* This is the combinator S *)
  end)

  module Monad = Preface_make.Monad.Via_bind (struct
    type nonrec 'a t = 'a t

    let return = pure

    let bind f ma s = map f ma s s
  end)
end
