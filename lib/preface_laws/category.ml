module type LAWS = sig
  type ('a, 'b) t

  val category_1 : unit -> (('a, 'b) t, ('a, 'b) t) Law.t
  val category_2 : unit -> (('a, 'b) t, ('a, 'b) t) Law.t
end

module For (C : Preface_specs.CATEGORY) : LAWS with type ('a, 'b) t := ('a, 'b) C.t =
struct
  open Law
  include Semigroupoid.For (C)

  let category_1 () =
    let lhs f = C.(f % id)
    and rhs f = f in
    law ("f % id" =~ lhs) ("f" =~ rhs)
  ;;

  let category_2 () =
    let lhs f = C.(id % f)
    and rhs f = f in
    law ("id % f" =~ lhs) ("f" =~ rhs)
  ;;
end
