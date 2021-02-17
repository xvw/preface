module Laws (A : Preface_specs.ARROW) = struct
  module C = Category.Laws (A)

  let right_identity = C.right_identity

  let left_identity = C.left_identity

  let associativity = C.associativity

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
    ( "fst f >>> arrow (fun (x,y) -> (x,g y)) = arrow (fun (x,y) -> (x,g y)) >>>\n\
      \     fst f"
    , (fun f g -> (left f g, right f g)) )
  ;;

  let law6 =
    let left f = A.(fst f >>> arrow Stdlib.fst)
    and right f = A.(arrow Stdlib.fst >>> f) in
    ( "fst f >>> arrow Stdlib.fst = arrow Stdlib.fst >>> f "
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
