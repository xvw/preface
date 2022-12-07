module type LAWS = sig
  module Semigroup : Preface_specs.SEMIGROUP

  val semigroup_1 :
    unit -> (Semigroup.t, Semigroup.t -> Semigroup.t -> Semigroup.t) Law.t
end

module For (S : Preface_specs.SEMIGROUP) : LAWS with module Semigroup := S =
struct
  open Law

  let semigroup_1 () =
    let lhs a b c = S.(Infix.(a <|> b) <|> c)
    and rhs a b c = S.(a <|> Infix.(b <|> c)) in

    law ("(a <|> b) <|> c" =~ lhs) ("a <|> (b <|> c)" =~ rhs)
  ;;
end
