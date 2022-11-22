let swap (a, b) = (b, a)
let assoc ((a, b), c) = (a, (b, c))
let unassoc (a, (b, c)) = ((a, b), c)
let swap_either x = Either.(fold ~left:right ~right:left) x
let curry f x y = f (x, y)
let uncurry f (x, y) = f x y

let assoc_either x =
  let open Either in
  match x with
  | Left (Left a) -> Left a
  | Left (Right b) -> Right (Left b)
  | Right c -> Right (Right c)
;;

let unassoc_either x =
  let open Either in
  match x with
  | Left a -> Left (Left a)
  | Right (Left b) -> Left (Right b)
  | Right (Right c) -> Right c
;;

module Fun = struct
  open Preface_core.Fun.Infix

  type ('a, 'b) t = 'a -> 'b

  module Profunctor = Preface_make.Profunctor.Via_dimap (struct
    type nonrec ('a, 'b) t = ('a, 'b) t

    let dimap x y z = y % z % x
  end)

  module Strong =
    Preface_make.Strong.Over_profunctor_via_fst
      (Profunctor)
      (struct
        type nonrec ('a, 'b) t = ('a, 'b) t

        let fst x (y, z) = (x y, z)
      end)

  module Choice =
    Preface_make.Choice.Over_profunctor_via_left
      (Profunctor)
      (struct
        type nonrec ('a, 'b) t = ('a, 'b) t

        let left f = function
          | Either.Left x -> Either.Left (f x)
          | Either.Right x -> Either.Right x
        ;;
      end)

  module Closed =
    Preface_make.Closed.Over_profunctor_via_closed
      (Profunctor)
      (struct
        type nonrec ('a, 'b) t = ('a, 'b) t

        let closed = Preface_core.Fun.compose_right_to_left
      end)
end

module Endo (T : Preface_specs.Types.T0) =
Preface_make.Monoid.Via_combine_and_neutral (struct
  open Preface_core.Fun.Infix

  type t = T.t -> T.t

  let neutral x = x
  let combine f g = f % g
end)

module Dual (T : Preface_specs.MONOID) =
Preface_make.Monoid.Via_combine_and_neutral (struct
  type t = T.t

  let neutral = T.neutral
  let combine a b = T.combine b a
end)

module Id = struct
  type 'a t = 'a

  module Functor = Preface_make.Functor.Via_map (struct
    type nonrec 'a t = 'a t

    let map f x = f x
  end)

  module Applicative = Preface_make.Applicative.Via_pure_and_apply (struct
    type nonrec 'a t = 'a t

    let pure x = x
    let apply f x = f x
  end)

  module Monad = Preface_make.Monad.Via_return_and_bind (struct
    type nonrec 'a t = 'a t

    let return x = x
    let bind f x = f x
  end)
end
