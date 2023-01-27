module type LAWS = sig
  type ('a, 'index) t

  val apply_1 :
    unit -> ((unit, 'index) t, ('a, 'index) t -> ('a, 'index) t) Law.t

  val apply_2 :
    unit -> (('a, 'index) t, (unit, 'index) t -> ('a, 'index) t) Law.t
end

module For (A : Preface_specs.INDEXED_APPLY) :
  LAWS with type ('a, 'index) t := ('a, 'index) A.t = struct
  open Law
  include Indexed_functor.For (A)

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
