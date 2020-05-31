module Over (M : Preface_specs.MONOID) = struct
  type log = M.t

  type 'a t = 'a * log

  let run = Preface_core.Fun.id

  let pure a = (a, M.neutral)

  let map f ma =
    let (a, s) = run ma in
    (f a, s)
  ;;

  module Functor = Preface_make.Functor.Via_map (struct
    type nonrec 'a t = 'a t

    let map = map
  end)

  module Applicative = Preface_make.Applicative.Via_apply (struct
    type nonrec 'a t = 'a t

    let pure = pure

    let apply mf ma =
      let (a, s') = run ma in
      let (f, s) = run mf in
      (f a, M.combine s s')
    ;;
  end)

  module Monad = Preface_make.Monad.Via_bind (struct
    type nonrec 'a t = 'a t

    let return = pure

    let bind f ma =
      let (a, s) = run ma in
      let (b, s') = run (f a) in
      (b, M.combine s s')
    ;;
  end)
end
