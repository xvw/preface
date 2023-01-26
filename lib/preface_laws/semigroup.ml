module type LAWS = sig
  type t

  val semigroup_1 :
    unit -> (t, t -> t -> t) Law.t
end

module For (S : Preface_specs.SEMIGROUP) : LAWS with type t := S.t =
struct
  open Law

  let semigroup_1 () =
    let lhs a b c = S.(Infix.(a <|> b) <|> c)
    and rhs a b c = S.(a <|> Infix.(b <|> c)) in

    law ("(a <|> b) <|> c" =~ lhs) ("a <|> (b <|> c)" =~ rhs)
  ;;
end
