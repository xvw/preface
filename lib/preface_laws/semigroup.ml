module type LAWS = sig
  module Semigroup : Preface_specs.SEMIGROUP

  val semigroup_associative_combine :
    unit -> (Semigroup.t, Semigroup.t -> Semigroup.t -> Semigroup.t) Law.t
end

module For (S : Preface_specs.SEMIGROUP) : LAWS with module Semigroup := S =
struct
  open Law

  let semigroup_associative_combine () =
    let lhs a b c = S.(Infix.(a <|> b) <|> c)
    and rhs a b c = S.(a <|> Infix.(b <|> c)) in

    law "Combine must be associative" ~lhs:("(a <|> b) <|> c" =~ lhs)
      ~rhs:("a <|> (b <|> c)" =~ rhs)
  ;;
end
