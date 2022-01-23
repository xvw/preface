open Aliases

module type LAWS = sig
  include Arrow.LAWS

  val law8 :
    string * (('a -> 'b) -> (('a, 'c) Either.t, ('b, 'c) Either.t) t pair)

  val law9 :
    string
    * (('a, 'b) t -> ('b, 'c) t -> (('a, 'd) Either.t, ('c, 'd) Either.t) t pair)

  val law10 : string * (('a, 'b) t -> ('a, ('b, 'c) Either.t) t pair)

  val law11 :
    string
    * (('a, 'b) t -> ('c -> 'd) -> (('a, 'c) Either.t, ('b, 'd) Either.t) t pair)

  val law12 :
    string
    * (   ('a, 'b) t
       -> ((('a, 'c) Either.t, 'd) Either.t, ('b, ('c, 'd) Either.t) Either.t) t
          pair )
end

module Laws (A : Preface_specs.ARROW_CHOICE) = struct
  include Arrow.Laws (A)

  let prod left right = Either.map ~left ~right

  let assoc = function
    | Either.Left (Either.Left x) -> Either.Left x
    | Either.Left (Either.Right x) -> Either.(Right (Left x))
    | Either.Right x -> Either.(Right (Right x))
  ;;

  let law8 =
    let lhs f = A.(left (arrow f)) in
    let rhs f = A.(arrow f +++ id) in
    ("left (arrow f) = arrow (f ++ id)", fun f -> (lhs f, rhs f))
  ;;

  let law9 =
    let lhs f g = A.(left (f >>> g)) in
    let rhs f g = A.(left f >>> left g) in
    ("left (f >>> g) = left f >>> left g", fun f g -> (lhs f g, rhs f g))
  ;;

  let law10 =
    let lhs f = A.(f >>> arrow Either.left) in
    let rhs f = A.(arrow Either.left >>> left f) in
    ("f >>> arr Left = arr Left >>> left f", fun f -> (lhs f, rhs f))
  ;;

  let law11 =
    let lhs f g = A.(left f >>> arrow (prod Fun.id g)) in
    let rhs f g = A.(arrow (prod Fun.id g) >>> left f) in
    ( "left f >>> arrow (Fun.id +++ g) = arrow (Fun.id +++ g) >>> left f"
    , fun f g -> (lhs f g, rhs f g) )
  ;;

  let law12 =
    let lhs f = A.(left (left f) >>> arrow assoc) in
    let rhs f = A.(arrow assoc >>> left f) in
    ( "left (left f) >>> arrow assocsum = arrow assocsum >>> left f"
    , fun f -> (lhs f, rhs f) )
  ;;
end
