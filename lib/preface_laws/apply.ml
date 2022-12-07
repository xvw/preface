module type LAWS = sig
  module Apply : Preface_specs.APPLY
  include Functor.LAWS with module Functor := Apply

  val apply_1 : unit -> (unit Apply.t, 'a Apply.t -> 'a Apply.t) Law.t
  val apply_2 : unit -> ('a Apply.t, unit Apply.t -> 'a Apply.t) Law.t
end

module For (A : Preface_specs.APPLY) : LAWS with module Apply := A = struct
  open Law
  include Functor.For (A)

  let apply_1 () =
    let lhs u v = A.(u *> v)
    and rhs u v = A.(Infix.(Fun.id <$ u) <*> v) in

    law ("u *> v" =~ lhs) ("(id <$ u) <*> v" =~ rhs)
  ;;

  let apply_2 () =
    let lhs u v = A.(u <* v)
    and rhs u v = A.(lift2 Fun.const u v) in

    law ("u <* v" =~ lhs) ("lift2 const u v" =~ rhs)
  ;;
end
