open Aliases

module type LAWS = sig
  include Profunctor.LAWS

  val contramap_fst_closed :
    string * (('a -> 'b) -> ('c, 'd) t -> ('b -> 'c, 'a -> 'd) t pair)

  val closed_closed :
    string * (('a, 'b) t -> ('c -> 'd -> 'a, 'c -> 'd -> 'b) t pair)

  val dimap_const : string * (('a, 'b) t -> ('a, 'b) t pair)
end

module Laws (C : Preface_specs.CLOSED) = struct
  include Profunctor.Laws (C)
  open Preface_core.Fun.Infix

  let contramap_fst_closed =
    let lhs f x = (C.contramap_fst (fun x -> x % f) % C.closed) x in
    let rhs f x = (C.map_snd (fun x -> x % f) % C.closed) x in
    ( "contramap_fst (fun x -> x % f) % closed = map_snd (fun x -> x % f) % \
       closed"
    , (fun f x -> (lhs f x, rhs f x)) )
  ;;

  let closed_closed =
    let lhs x = (C.closed % C.closed) x in
    let rhs x =
      (C.dimap (fun f (x, y) -> f x y) (fun f x y -> f (x, y)) % C.closed) x
    in
    ("closed % closed = dimap uncurry curry % closed", (fun x -> (lhs x, rhs x)))
  ;;

  let dimap_const =
    let f g = g () in
    let lhs x = (C.dimap Fun.const f % C.closed) x in
    let rhs x = Fun.id x in
    ("dimap const (fun f -> f ()) % closed = id", (fun x -> (lhs x, rhs x)))
  ;;
end
