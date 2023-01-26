module type LAWS = sig
  type ('a, 'b) t

  val profunctor_1 : unit -> (('a, 'b) t, ('a, 'b) t) Law.t
  val profunctor_2 : unit -> (('a, 'b) t, ('a, 'b) t) Law.t
  val profunctor_3 : unit -> (('a, 'b) t, ('a, 'b) t) Law.t

  val profunctor_4 :
    unit -> ('a -> 'b, ('c -> 'd) -> ('b, 'c) t -> ('a, 'd) t) Law.t

  val profunctor_5 :
       unit
    -> ( 'a -> 'b
       , ('c -> 'a) -> ('d -> 'e) -> ('f -> 'd) -> ('b, 'f) t -> ('c, 'e) t )
       Law.t

  val profunctor_6 :
    unit -> ('a -> 'b, ('c -> 'a) -> ('b, 'd) t -> ('c, 'd) t) Law.t

  val profunctor_7 :
    unit -> ('a -> 'b, ('c -> 'a) -> ('d, 'c) t -> ('d, 'b) t) Law.t
end

module For (P : Preface_specs.PROFUNCTOR) :
  LAWS with type ('a, 'b) t := ('a, 'b) P.t = struct
  open Law
  open Preface_core.Fun.Infix

  let profunctor_1 () =
    let lhs x = P.dimap (fun x -> x) (fun x -> x) x
    and rhs x = x in

    law ("dimap id id" =~ lhs) ("id" =~ rhs)
  ;;

  let profunctor_2 () =
    let lhs x = P.contramap_fst (fun x -> x) x
    and rhs x = x in

    law ("contramap_fst id" =~ lhs) ("id" =~ rhs)
  ;;

  let profunctor_3 () =
    let lhs x = P.map_snd (fun x -> x) x
    and rhs x = x in

    law ("map_snd id" =~ lhs) ("id" =~ rhs)
  ;;

  let profunctor_4 () =
    let lhs f g x = P.dimap f g x
    and rhs f g x = (P.contramap_fst f % P.map_snd g) x in

    law ("dimap f g" =~ lhs) ("contramap_fst f % map_snd g" =~ rhs)
  ;;

  let profunctor_5 () =
    let lhs f g h i x = P.dimap (f % g) (h % i) x
    and rhs f g h i x = (P.dimap g h % P.dimap f i) x in
    law ("dimap (f % g) (h % i)" =~ lhs) ("dimap f h % dimap g i" =~ rhs)
  ;;

  let profunctor_6 () =
    let lhs f g x = P.contramap_fst (f % g) x
    and rhs f g x = (P.contramap_fst g % P.contramap_fst f) x in
    law
      ("contramap_fst (f % g)" =~ lhs)
      ("contramap_fst f % contramap_fst g" =~ rhs)
  ;;

  let profunctor_7 () =
    let lhs f g x = P.map_snd (f % g) x
    and rhs f g x = (P.map_snd f % P.map_snd g) x in
    law ("map_snd (f % g)" =~ lhs) ("map_snd f % map_snd g" =~ rhs)
  ;;
end
