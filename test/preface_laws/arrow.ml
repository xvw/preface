open Aliases

module type LAWS = sig
  include Category.LAWS

  val law1 : string * (unit -> ('a, 'a) t pair)

  val law2 : string * (('a -> 'b) -> ('b -> 'c) -> ('a, 'c) t pair)

  val law3 : string * (('a -> 'b) -> ('a * 'c, 'b * 'c) t pair)

  val law4 : string * (('a, 'b) t -> ('b, 'c) t -> ('a * 'd, 'c * 'd) t pair)

  val law5 : string * (('a, 'b) t -> ('c -> 'd) -> ('a * 'c, 'b * 'd) t pair)

  val law6 : string * (('a, 'b) t -> ('a * 'c, 'b) t pair)

  val law7 : string * (('a, 'b) t -> (('a * 'c) * 'd, 'b * ('c * 'd)) t pair)
end

module Laws (A : Preface_specs.ARROW) = struct
  include Category.Laws (A)

  let law1 = ("arrow id = id", (fun () -> (A.arrow (fun x -> x), A.id)))

  let law2 =
    let left f g =
      let h = Preface_core.Fun.(g % f) in
      A.(arrow h)
    and right f g = A.(arrow f >>> arrow g) in
    ("arrow (g % f) = arrow f >>> arrow g", (fun f g -> (left f g, right f g)))
  ;;

  let law3 =
    let left f = A.(fst (arrow f))
    and right f = A.arrow (fun (x, y) -> (f x, y)) in
    ("fst (arrow f) = arr (fst f)", (fun f -> (left f, right f)))
  ;;

  let law4 =
    let left f g = A.(fst (f >>> g))
    and right f g = A.(fst f >>> fst g) in
    ("fst (f >>> g) = fst f >>> fst g", (fun f g -> (left f g, right f g)))
  ;;

  let law5 =
    let left f g = A.(fst f >>> arrow (fun (x, y) -> (x, g y)))
    and right f g = A.(arrow (fun (x, y) -> (x, g y)) >>> fst f) in
    ( "fst f >>> arrow (fun (x,y) -> (x,g y)) = arrow (fun (x,y) -> (x,g y)) \
       >>> fst f"
    , (fun f g -> (left f g, right f g)) )
  ;;

  let law6 =
    let left f = A.(fst f >>> arrow Stdlib.fst)
    and right f = A.(arrow Stdlib.fst >>> f) in
    ( "fst f >>> arrow Stdlib.fst = arrow Stdlib.fst >>> f"
    , (fun f -> (left f, right f)) )
  ;;

  let law7 =
    let assoc ((x, y), z) = (x, (y, z)) in
    let left f = A.(fst (fst f) >>> arrow assoc)
    and right f = A.(arrow assoc >>> fst f) in
    ( "fst (fst f) >>> arrow assoc = arrow assoc >>> fst f"
    , (fun f -> (left f, right f)) )
  ;;
end
