module type LAWS = sig
  module Arrow : Preface_specs.ARROW
  include Category.LAWS with module Category := Arrow

  val arrow_1 : unit -> (unit, ('a, 'a) Arrow.t) Law.t
  val arrow_2 : unit -> ('a -> 'b, ('b -> 'c) -> ('a, 'c) Arrow.t) Law.t
  val arrow_3 : unit -> ('a -> 'b, ('a * 'c, 'b * 'c) Arrow.t) Law.t

  val arrow_4 :
       unit
    -> (('a, 'b) Arrow.t, ('b, 'c) Arrow.t -> ('a * 'd, 'c * 'd) Arrow.t) Law.t

  val arrow_5 : unit -> (('a, 'b) Arrow.t, ('a * 'c, 'b) Arrow.t) Law.t

  val arrow_6 :
    unit -> (('a, 'b) Arrow.t, ('c -> 'd) -> ('a * 'c, 'b * 'd) Arrow.t) Law.t

  val arrow_7 :
    unit -> (('a, 'b) Arrow.t, (('a * 'c) * 'd, 'b * ('c * 'd)) Arrow.t) Law.t
end

module For (A : Preface_specs.ARROW) : LAWS with module Arrow := A = struct
  open Law
  include Category.For (A)

  let arrow_1 () =
    let lhs () = A.arrow (fun x -> x)
    and rhs () = A.id in

    law ("arrow Fun.id" =~ lhs) ("id" =~ rhs)
  ;;

  let arrow_2 () =
    let lhs f g = A.arrow Preface_core.Fun.Infix.(g % f)
    and rhs f g = A.(arrow f >>> arrow g) in

    law ("arrow (fun x -> f (g x))" =~ lhs) ("arrow f >>> arrow g" =~ rhs)
  ;;

  let arrow_3 () =
    let lhs f = A.(fst (arrow f))
    and rhs f = A.(arrow (fun (x, y) -> (f x, y))) in

    law ("fst (arrow f)" =~ lhs) ("arrow (fun (x, y) -> (f x, y))" =~ rhs)
  ;;

  let arrow_4 () =
    let lhs f g = A.(fst (f >>> g))
    and rhs f g = A.(fst f >>> fst g) in

    law ("fst (f >>> g)" =~ lhs) ("fst f >>> fst g" =~ rhs)
  ;;

  let arrow_5 () =
    let lhs f = A.(fst f >>> arrow Stdlib.fst)
    and rhs f = A.(arrow Stdlib.fst >>> f) in

    law ("fst f >>> arrow Stdlib.fst" =~ lhs) ("arrow Stdlib.fst >>> f" =~ rhs)
  ;;

  let arrow_6 () =
    let lhs f g = A.(fst f >>> arrow Util.Fun.Arrow.(id *** g))
    and rhs f g = A.(arrow Util.Fun.Arrow.(id *** g) >>> fst f) in

    law
      ("fst f >>> arrow Fun.Arrow.(id *** g)" =~ lhs)
      ("arrow Fun.Arrow.(id *** g) >>> fst g" =~ rhs)
  ;;

  let arrow_7 () =
    let lhs f = A.(fst (fst f) >>> arrow Util.assoc)
    and rhs f = A.(arrow Util.assoc >>> fst f) in

    law ("(fst (fst f)) >>> arrow assoc" =~ lhs) ("arrow assoc >>> fst f" =~ rhs)
  ;;
end
