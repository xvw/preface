module Via_type (T : sig
  type t
end) =
struct
  type state = T.t

  type 'a t = state -> 'a * state

  let run s ma = ma s

  let pure a s = (a, s)

  let map f ma s =
    let (a, s') = ma s in
    (f a, s')
  ;;

  module Functor = Preface_make.Functor.Via_map (struct
    type nonrec 'a t = 'a t

    let map = map
  end)

  module Applicative = Preface_make.Applicative.Via_apply (struct
    type nonrec 'a t = 'a t

    let pure = pure

    let apply mf ma s =
      let (f, s') = run s mf in
      map f ma s'
    ;;
  end)

  module Monad = Preface_make.Monad.Via_bind (struct
    type nonrec 'a t = 'a t

    let return = pure

    let bind f ma s =
      let (a, s') = run s ma in
      run s' @@ f a
    ;;
  end)

  let get s = (s, s)

  let set s _ = ((), s)

  let modify f = Monad.(get >>= (fun s -> set @@ f s))
end
