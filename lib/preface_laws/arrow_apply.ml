module type LAWS = sig
  type ('a, 'b) t

  val arrow_apply_1 : unit -> (unit, ('a * 'b, 'a * 'b) t) Law.t

  val arrow_apply_2 :
       unit
    -> ( ('a, 'b) t
       , (('b, 'c) t * 'a, 'c) t )
       Law.t

  val arrow_apply_3 :
       unit
    -> ( ('a, 'b) t
       , (('c, 'a) t * 'c, 'b) t )
       Law.t
end

module For (A : Preface_specs.ARROW_APPLY) : LAWS with type ('a, 'b) t := ('a, 'b) A.t =
struct
  open Law
  include Arrow.For (A)

  let arrow_apply_1 () =
    let lhs () = A.(fst (arrow (fun x -> arrow (fun y -> (x, y)))) >>> apply)
    and rhs () = A.id in

    law
      ("fst (arrow (fun x -> arrow (fun y -> (x, y)))) >>> apply" =~ lhs)
      ("id" =~ rhs)
  ;;

  let arrow_apply_2 () =
    let lhs g = A.(fst (arrow (fun x -> g >>> x)) >>> apply)
    and rhs g = A.(snd g >>> apply) in

    law
      ("fst (arrow (fun x -> g >>> x)) >>> apply" =~ lhs)
      ("snd g >>> apply" =~ rhs)
  ;;

  let arrow_apply_3 () =
    let lhs h = A.(fst (arrow (fun x -> x >>> h)) >>> apply)
    and rhs h = A.(apply >>> h) in

    law
      ("fst (arrow (fun x -> x >>> h)) >>> apply" =~ lhs)
      ("apply >>> h" =~ rhs)
  ;;
end
