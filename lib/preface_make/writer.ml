module Over (W : Preface_specs.MONOID) = struct

  type output = W.t

  type 'a t = 'a * output

  let run = Preface_core.Fun.id

  let pure a = (a, W.neutral)

  let map f ma =
    let (a, s) = run ma in
    (f a, s)
  ;;

  module Functor = Functor.Via_map (struct
    type nonrec 'a t = 'a t

    let map = map
  end)

  module Applicative = Applicative.Via_apply (struct
    type nonrec 'a t = 'a t

    let pure = pure

    let apply mf ma =
      let (a, s') = run ma in
      let (f, s) = run mf in
      (f a, W.combine s s')
    ;;
  end)

  module Monad = Monad.Via_bind (struct
    type nonrec 'a t = 'a t

    let return = pure

    let bind f ma =
      let (a, s) = run ma in
      let (b, s') = run (f a) in
      (b, W.combine s s')
    ;;
  end)

  let tell s = ((), s)

  let write (a,s) =
    let open Monad in
    let* _ = tell s in
    return a

  let listen ma =
    let (a, s) = run ma in
    ((a, s), s)
  ;;

  let pass ma =
    let ((a, f), s) = run ma in
    (a, f s)
  ;;

  let listens f ma =
    let ((a,s),s') = listen ma in
    ((a, f s), s')
  ;;

end
