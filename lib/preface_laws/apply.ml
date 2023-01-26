module type LAWS = sig
  type 'a t

  val apply_1 : unit -> (unit t, 'a t -> 'a t) Law.t
  val apply_2 : unit -> ('a t, unit t -> 'a t) Law.t
end

module For (A : Preface_specs.APPLY) : LAWS with type 'a t := 'a A.t = struct
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
