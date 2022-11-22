module type LAWS = sig
  module Decidable : Preface_specs.DECIDABLE
  include Divisible.LAWS with module Divisible := Decidable

  val decidable_left_identity :
    unit -> ('a -> Preface_core.Void.t, 'a Decidable.t -> 'a Decidable.t) Law.t

  val decidable_right_identity :
    unit -> ('a -> Preface_core.Void.t, 'a Decidable.t -> 'a Decidable.t) Law.t
end

module For (D : Preface_specs.DECIDABLE) : LAWS with module Decidable := D =
struct
  open Law
  include Divisible.For (D)

  let decidable_left_identity () =
    let lhs f m = D.choose Either.left m (D.lose f)
    and rhs _ m = m in

    law "left identity"
      ~lhs:("choose Either.left m (lose f)" =~ lhs)
      ~rhs:("m" =~ rhs)
  ;;

  let decidable_right_identity () =
    let lhs f m = D.choose Either.right (D.lose f) m
    and rhs _ m = m in

    law "right identity"
      ~lhs:("choose Either.right (lose f) m" =~ lhs)
      ~rhs:("m" =~ rhs)
  ;;
end
