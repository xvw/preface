module type LAWS = sig
  module Monoid : Preface_specs.MONOID
  include Semigroup.LAWS with module Semigroup := Monoid

  val monoid_1 : unit -> (Monoid.t, Monoid.t) Law.t
  val monoid_2 : unit -> (Monoid.t, Monoid.t) Law.t
end

module For (M : Preface_specs.MONOID) : LAWS with module Monoid := M = struct
  open Law
  include Semigroup.For (M)

  let monoid_1 () =
    let lhs x = M.(combine neutral x)
    and rhs x = x in

    law ("neutral <|> x" =~ lhs) ("x" =~ rhs)
  ;;

  let monoid_2 () =
    let lhs x = M.(combine x neutral) in
    let rhs x = x in

    law ("x <|> neutral" =~ lhs) ("x" =~ rhs)
  ;;
end
