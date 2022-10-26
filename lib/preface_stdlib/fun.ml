type ('a, 'b) t = 'a -> 'b

include Preface_core.Fun

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

module Semigroupoid = Preface_make.Semigroupoid.Via_compose (struct
  type nonrec ('a, 'b) t = ('a, 'b) t

  let compose = compose_right_to_left
end)

module Category =
  Preface_make.Category.Over_semigroupoid
    (Semigroupoid)
    (struct
      type nonrec ('a, 'b) t = ('a, 'b) t

      let id x = x
    end)

module Arrow = Preface_make.Arrow.From_strong_and_category (Strong) (Category)

module Arrow_choice =
  Preface_make.Arrow_choice.Over_arrow_with_choose
    (Arrow)
    (struct
      type nonrec ('a, 'b) t = ('a, 'b) t

      let case f g = Stdlib.Either.fold ~left:f ~right:g
      let choose f g = case (Stdlib.Either.left % f) (Stdlib.Either.right % g)
    end)

module Arrow_apply =
  Preface_make.Arrow_apply.Over_arrow
    (Arrow)
    (struct
      type nonrec ('a, 'b) t = ('a, 'b) t

      let apply (f, x) = f x
    end)
