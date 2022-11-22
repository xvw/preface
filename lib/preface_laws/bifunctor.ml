module type LAWS = sig
  module Bifunctor : Preface_specs.BIFUNCTOR

  val bifunctor_1 : unit -> (('a, 'b) Bifunctor.t, ('a, 'b) Bifunctor.t) Law.t
  val bifunctor_2 : unit -> (('a, 'b) Bifunctor.t, ('a, 'b) Bifunctor.t) Law.t
  val bifunctor_3 : unit -> (('a, 'b) Bifunctor.t, ('a, 'b) Bifunctor.t) Law.t

  val bifunctor_4 :
       unit
    -> ( 'a -> 'b
       , ('c -> 'd) -> ('a, 'c) Bifunctor.t -> ('b, 'd) Bifunctor.t )
       Law.t

  val bifunctor_5 :
       unit
    -> ( 'a -> 'b
       ,    ('c -> 'a)
         -> ('d -> 'e)
         -> ('f -> 'd)
         -> ('c, 'f) Bifunctor.t
         -> ('b, 'e) Bifunctor.t )
       Law.t

  val bifunctor_6 :
       unit
    -> ( 'a -> 'b
       , ('c -> 'a) -> ('c, 'd) Bifunctor.t -> ('b, 'd) Bifunctor.t )
       Law.t

  val bifunctor_7 :
       unit
    -> ( 'a -> 'b
       , ('c -> 'a) -> ('d, 'c) Bifunctor.t -> ('d, 'b) Bifunctor.t )
       Law.t
end

module For (B : Preface_specs.BIFUNCTOR) : LAWS with module Bifunctor := B =
struct
  open Law
  open Preface_core.Fun.Infix

  let bifunctor_1 () =
    let lhs x = B.bimap (fun x -> x) (fun x -> x) x
    and rhs x = x in

    law ("bimap id id" =~ lhs) ("id" =~ rhs)
  ;;

  let bifunctor_2 () =
    let lhs x = B.map_fst (fun x -> x) x
    and rhs x = x in

    law ("map_fst id" =~ lhs) ("id" =~ rhs)
  ;;

  let bifunctor_3 () =
    let lhs x = B.map_snd (fun x -> x) x
    and rhs x = x in

    law ("map_snd id" =~ lhs) ("id" =~ rhs)
  ;;

  let bifunctor_4 () =
    let lhs f g x = B.bimap f g x
    and rhs f g x = (B.map_fst f % B.map_snd g) x in

    law ("bimap f g" =~ lhs) ("map_fst f % map_snd g" =~ rhs)
  ;;

  let bifunctor_5 () =
    let lhs f g h i x = B.bimap (f % g) (h % i) x
    and rhs f g h i x = (B.bimap f h % B.bimap g i) x in

    law ("bimap (f % g) (h % i)" =~ lhs) ("bimap f h % bimap g i" =~ rhs)
  ;;

  let bifunctor_6 () =
    let lhs f g x = B.map_fst (f % g) x
    and rhs f g x = (B.map_fst f % B.map_fst g) x in

    law ("map_fst (f % g)" =~ lhs) ("map_fst f % map_fst g" =~ rhs)
  ;;

  let bifunctor_7 () =
    let lhs f g x = B.map_snd (f % g) x
    and rhs f g x = (B.map_snd f % B.map_snd g) x in

    law ("map_snd (f % g)" =~ lhs) ("map_snd f % map_snd g" =~ rhs)
  ;;
end
