module type LAWS = sig
  module Monoid : Preface_specs.MONOID
  include Semigroup.LAWS with module Semigroup := Monoid

  val monoid_left_identity : unit -> (Monoid.t, Monoid.t) Law.t
  val monoid_right_identity : unit -> (Monoid.t, Monoid.t) Law.t
end

module For (M : Preface_specs.MONOID) : LAWS with module Monoid := M = struct
  open Law
  include Semigroup.For (M)

  let monoid_left_identity () =
    let lhs x = M.(combine neutral x)
    and rhs x = x in

    law "Left identity" ~lhs:("neutral <|> x" =~ lhs) ~rhs:("x" =~ rhs)
  ;;

  let monoid_right_identity () =
    let lhs x = M.(combine x neutral) in
    let rhs x = x in

    law "Right identity" ~lhs:("x <|> neutral" =~ lhs) ~rhs:("x" =~ rhs)
  ;;
end
