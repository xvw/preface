module Over
    (S : Preface_specs.Types.T0)
    (P : Preface_specs.MONOID) (R : sig
      val right : P.t -> S.t -> S.t
    end) =
struct
  type env = S.t

  type output = P.t

  type 'a t = env -> 'a * output

  let pure a _ = (a, P.neutral)

  module Applicative = Applicative.Via_apply (struct
    type nonrec 'a t = 'a t

    let pure = pure

    let apply mf ma s =
      let (f, p) = mf s in
      let (a, p') = ma (R.right p s) in
      (f a, P.combine p p')
    ;;
  end)

  module Monad = Monad.Via_bind (struct
    type nonrec 'a t = 'a t

    let return = pure

    let bind f ma s =
      let (a, p) = ma s in
      let t = f a in
      let (ma', p') = t (R.right p s) in
      (ma', P.combine p p')
    ;;
  end)

  module Functor = Functor.Via_map (Monad)
end
