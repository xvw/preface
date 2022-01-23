type 'a t = 'a -> bool

module Contravariant = Preface_make.Contravariant.Via_contramap (struct
  type nonrec 'a t = 'a t

  let contramap f g = Preface_core.Fun.(g % f)
end)

module Invariant = Preface_make.Invariant.From_contravariant (Contravariant)

module Divisible =
  Preface_make.Divisible.Over_contravariant
    (Contravariant)
    (struct
      type nonrec 'a t = 'a t

      let divide f pa pb x =
        let a, b = f x in
        pa a && pb b
      ;;

      let conquer _ = true
    end)

module Decidable =
  Preface_make.Decidable.Over_divisible
    (Divisible)
    (struct
      type nonrec 'a t = 'a t

      let choose f pa pb c =
        Preface_core.Shims.Either.fold ~left:pa ~right:pb (f c)
      ;;

      let lose a x = Void.absurd (a x)
    end)

let negate p x = not (p x)
let tautology _ = true
let contradiction _ = false
let and_ l r x = l x && r x
let or_ l r x = l x || r x

module Infix = struct
  let ( && ) = and_
  let ( || ) = or_
  let ( ! ) = negate
end

include Infix
