open Aliases
open Preface_core.Shims

module type LAWS = sig
  include Profunctor.LAWS

  val left_defined_by_right :
    string * (('a, 'b) t -> (('a, 'c) Either.t, ('b, 'c) Either.t) t pair)

  val right_defined_by_left :
    string * (('a, 'b) t -> (('c, 'a) Either.t, ('c, 'b) Either.t) t pair)

  val map_snd_left : string * (('a, 'b) t -> ('a, ('b, 'c) Either.t) t pair)
  val map_snd_right : string * (('a, 'b) t -> ('a, ('c, 'b) Either.t) t pair)

  val contramap_fst_right :
    string
    * (('a -> 'b) -> ('c, 'd) t -> (('c, 'a) Either.t, ('d, 'b) Either.t) t pair)

  val contramap_fst_left :
    string
    * (('a -> 'b) -> ('c, 'd) t -> (('a, 'c) Either.t, ('b, 'd) Either.t) t pair)

  val left_left :
    string
    * (   ('a, 'b) t
       -> ((('a, 'c) Either.t, 'd) Either.t, (('b, 'c) Either.t, 'd) Either.t) t
          pair )

  val right_right :
    string
    * (   ('a, 'b) t
       -> (('c, ('d, 'a) Either.t) Either.t, ('c, ('d, 'b) Either.t) Either.t) t
          pair )
end

module Laws (C : Preface_specs.CHOICE) = struct
  include Profunctor.Laws (C)

  let assoc x =
    let open Either in
    match x with
    | Left (Left a) -> Left a
    | Left (Right b) -> Right (Left b)
    | Right c -> Right (Right c)
  ;;

  let unassoc x =
    let open Either in
    match x with
    | Left a -> Left (Left a)
    | Right (Left b) -> Left (Right b)
    | Right (Right c) -> Right c
  ;;

  let left_defined_by_right =
    let lhs x = C.left x in
    let rhs x = C.dimap Either.swap Either.swap (C.right x) in
    ("left = dimap Either.swap Either.swap % right", fun x -> (lhs x, rhs x))
  ;;

  let right_defined_by_left =
    let lhs x = C.right x in
    let rhs x = C.dimap Either.swap Either.swap (C.left x) in
    ("right = dimap Either.swap Either.swap % left", fun x -> (lhs x, rhs x))
  ;;

  let map_snd_left =
    let open Preface_core.Fun.Infix in
    let lhs x = C.map_snd Either.left x in
    let rhs x = (C.contramap_fst Either.left % C.left) x in
    ( "map_snd Either.left = contramap_fst Either.left % left"
    , fun x -> (lhs x, rhs x) )
  ;;

  let map_snd_right =
    let open Preface_core.Fun.Infix in
    let lhs x = C.map_snd Either.right x in
    let rhs x = (C.contramap_fst Either.right % C.right) x in
    ( "map_snd Either.right = contramap_fst Either.right % right"
    , fun x -> (lhs x, rhs x) )
  ;;

  let contramap_fst_right =
    let open Preface_core.Fun.Infix in
    let right = Preface_stdlib.Fun.Choice.right in
    let lhs f x = (C.contramap_fst (right f) % C.left) x in
    let rhs f x = (C.map_snd (right f) % C.left) x in
    ( "contramap_fst (Fun.Choice.right f) % left = map_snd (Fun.Choice.right \
       f) % left"
    , fun f x -> (lhs f x, rhs f x) )
  ;;

  let contramap_fst_left =
    let open Preface_core.Fun.Infix in
    let left = Preface_stdlib.Fun.Choice.left in
    let lhs f x = (C.contramap_fst (left f) % C.right) x in
    let rhs f x = (C.map_snd (left f) % C.right) x in
    ( "contramap_fst (Fun.Choice.left f) % right = map_snd (Fun.Choice.left f) \
       % right"
    , fun f x -> (lhs f x, rhs f x) )
  ;;

  let left_left =
    let open Preface_core.Fun.Infix in
    let lhs x = (C.left % C.left) x in
    let rhs x = (C.dimap assoc unassoc % C.left) x in
    ("left % left = dimap assoc unassoc % left", fun x -> (lhs x, rhs x))
  ;;

  let right_right =
    let open Preface_core.Fun.Infix in
    let lhs x = (C.right % C.right) x in
    let rhs x = (C.dimap unassoc assoc % C.right) x in
    ("left % left = dimap unassoc assoc % right", fun x -> (lhs x, rhs x))
  ;;
end
