open Aliases

module type LAWS = sig
  include Arrow.LAWS

  val law8 : string * (unit -> ('a * 'b, 'a * 'b) t pair)

  val law9 : string * (('a, 'b) t -> (('b, 'c) t * 'a, 'c) t pair)

  val law10 : string * (('a, 'b) t -> (('c, 'a) t * 'c, 'b) t pair)
end

module Laws (A : Preface_specs.ARROW_APPLY) = struct
  include Arrow.Laws (A)

  let law8 =
    let lhs () = A.(fst (arrow (fun x -> arrow (fun y -> (x, y)))) >>> apply) in
    let rhs () = A.id in
    ( "fst (arrow (fun x -> arrow (fun y -> (x,y)))) >>> apply = id"
    , (fun () -> (lhs (), rhs ())) )
  ;;

  let law9 =
    let lhs g = A.(fst (arrow (fun x -> g >>> x)) >>> apply) in
    let rhs g = A.(snd g >>> apply) in
    ( "fst (arrow (fun x -> g >>> x)) >>> apply = snd g >>> apply"
    , (fun g -> (lhs g, rhs g)) )
  ;;

  let law10 =
    let lhs h = A.(fst (arrow (fun x -> x >>> h)) >>> apply) in
    let rhs h = A.(apply >>> h) in
    ( "fst (arrow (fun x -> x >>> h)) >>> apply = apply >>> h"
    , (fun h -> (lhs h, rhs h)) )
  ;;
end
