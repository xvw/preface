module type LAWS = sig
  module Closed : Preface_specs.CLOSED
  include Profunctor.LAWS with module Profunctor := Closed

  val closed_1 :
    unit -> ('a -> 'b, ('c, 'd) Closed.t -> ('b -> 'c, 'a -> 'd) Closed.t) Law.t

  val closed_2 :
    unit -> (('a, 'b) Closed.t, ('c -> 'd -> 'a, 'c -> 'd -> 'b) Closed.t) Law.t

  val closed_3 : unit -> (('a, 'b) Closed.t, ('a, 'b) Closed.t) Law.t
end

module For (C : Preface_specs.CLOSED) : LAWS with module Closed := C = struct
  open Law
  open Preface_core.Fun.Infix
  include Profunctor.For (C)

  let closed_1 () =
    let lhs f x = C.contramap_fst (fun x -> x % f) (C.closed x)
    and rhs f x = C.map_snd (fun x -> x % f) (C.closed x) in

    law
      ("contrampa_fs (fun x -> x % f) % closed" =~ lhs)
      ("map_snd (fun x -> x % f) % closed" =~ rhs)
  ;;

  let closed_2 () =
    let lhs x = C.closed (C.closed x)
    and rhs x = C.dimap Util.uncurry Util.curry (C.closed x) in

    law ("closed % closed" =~ lhs) ("dimap uncurry curry % closed" =~ rhs)
  ;;

  let closed_3 () =
    let lhs x = C.dimap Fun.const (fun f -> f ()) (C.closed x)
    and rhs x = x in

    law ("dimap const (fun f -> f ()) % closed" =~ lhs) ("id" =~ rhs)
  ;;
end
