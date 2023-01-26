module type LAWS = sig
  type t

  val monoid_1 : unit -> (t, t) Law.t
  val monoid_2 : unit -> (t, t) Law.t
end

module For (M : Preface_specs.MONOID) : LAWS with type t := M.t = struct
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
