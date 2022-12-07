type 'a t = 'a -> 'a -> bool

module Contravariant = Preface_make.Contravariant.Via_contramap (struct
  type nonrec 'a t = 'a t

  let contramap f g x y = g (f x) (f y)
end)

module Invariant = Preface_make.Invariant.From_contravariant (Contravariant)

module Divisible =
  Preface_make.Divisible.Over_contravariant
    (Contravariant)
    (struct
      type nonrec 'a t = 'a t

      let conquer _ _ = true

      let divide f g h a b =
        let w, x = f a in
        let y, z = f b in
        g w y && h x z
      ;;
    end)

module Decidable =
  Preface_make.Decidable.Over_divisible
    (Divisible)
    (struct
      type nonrec 'a t = 'a t

      let lose f x = Preface_core.Void.absurd (f x)

      let choose f g h a b =
        Stdlib.Either.fold
          ~left:(fun c ->
            match f b with
            | Stdlib.Either.Left d -> g c d
            | Stdlib.Either.Right _ -> false )
          ~right:(fun c ->
            match f b with
            | Stdlib.Either.Left _ -> false
            | Stdlib.Either.Right d -> h c d )
          (f a)
      ;;
    end)

let negate e x y = not (e x y)
