module Over (T : Preface_specs.Types.T0) = struct
  type state = T.t

  type 'a t = state -> 'a * state

  let run ma s = ma s

  let pure a s = (a, s)

  let map f ma s =
    let (a, s') = run ma s in
    (f a, s')
  ;;

  module Functor = Functor.Via_map (struct
    type nonrec 'a t = 'a t

    let map = map
  end)

  module Applicative = Applicative.Via_apply (struct
    type nonrec 'a t = 'a t

    let pure = pure

    let apply mf ma s =
      let (f, s') = run mf s in
      map f ma s'
    ;;
  end)

  module Monad = Monad.Via_bind (struct
    type nonrec 'a t = 'a t

    let return = pure

    let bind f ma s =
      let (a, s') = run ma s in
      run (f a) s'
    ;;
  end)

  let get s = (s, s)

  let set s _ = ((), s)

  let modify f = Monad.(get >>= (fun s -> set (f s)))
end
