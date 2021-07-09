module type LAWS = sig
  module Apply : Preface_specs.APPLY
  include Functor.LAWS with module Functor := Apply

  val apply_ignore_left : unit -> (unit Apply.t, 'a Apply.t -> 'a Apply.t) Law.t

  val apply_ignore_right :
    unit -> ('a Apply.t, unit Apply.t -> 'a Apply.t) Law.t
end

module For (A : Preface_specs.APPLY) : LAWS with module Apply := A = struct
  open Law
  include Functor.For (A)

  let apply_ignore_left () =
    let lhs u v = A.(u *> v)
    and rhs u v = A.(Infix.(Fun.id <$ u) <*> v) in

    law "Ignore left" ~lhs:("u *> v" =~ lhs) ~rhs:("(id <$ u) <*> v" =~ rhs)
  ;;

  let apply_ignore_right () =
    let lhs u v = A.(u <* v)
    and rhs u v = A.(lift2 Fun.const u v) in

    law "Ignore right" ~lhs:("u <* v" =~ lhs) ~rhs:("lift2 const u v" =~ rhs)
  ;;
end
