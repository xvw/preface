module type LAWS = sig
  module Decidable : Preface_specs.DECIDABLE
  include Divisible.LAWS with module Divisible := Decidable

  val decidable_1 :
    unit -> ('a -> Preface_core.Void.t, 'a Decidable.t -> 'a Decidable.t) Law.t

  val decidable_2 :
    unit -> ('a -> Preface_core.Void.t, 'a Decidable.t -> 'a Decidable.t) Law.t
end

module For (D : Preface_specs.DECIDABLE) : LAWS with module Decidable := D =
struct
  open Law
  include Divisible.For (D)

  let decidable_1 () =
    let lhs f m = D.choose Either.left m (D.lose f)
    and rhs _ m = m in

    law ("choose Either.left m (lose f)" =~ lhs) ("m" =~ rhs)
  ;;

  let decidable_2 () =
    let lhs f m = D.choose Either.right (D.lose f) m
    and rhs _ m = m in

    law ("choose Either.right (lose f) m" =~ lhs) ("m" =~ rhs)
  ;;
end
