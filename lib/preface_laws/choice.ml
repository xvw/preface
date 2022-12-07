module type LAWS = sig
  module Choice : Preface_specs.CHOICE
  include Profunctor.LAWS with module Profunctor := Choice

  val choice_1 :
       unit
    -> ( ('a, 'b) Choice.t
       , (('a, 'c) Either.t, ('b, 'c) Either.t) Choice.t )
       Law.t

  val choice_2 :
    unit -> (('a, 'b) Choice.t, ('a, ('b, 'c) Either.t) Choice.t) Law.t

  val choice_3 :
       unit
    -> ( 'a -> 'b
       , ('c, 'd) Choice.t -> (('c, 'a) Either.t, ('d, 'b) Either.t) Choice.t
       )
       Law.t

  val choice_4 :
       unit
    -> ( ('a, 'b) Choice.t
       , ( (('a, 'c) Either.t, 'd) Either.t
         , (('b, 'c) Either.t, 'd) Either.t )
         Choice.t )
       Law.t

  val choice_5 :
       unit
    -> ( ('a, 'b) Choice.t
       , (('c, 'a) Either.t, ('c, 'b) Either.t) Choice.t )
       Law.t

  val choice_6 :
    unit -> (('a, 'b) Choice.t, ('a, ('c, 'b) Either.t) Choice.t) Law.t

  val choice_7 :
       unit
    -> ( 'a -> 'b
       , ('c, 'd) Choice.t -> (('a, 'c) Either.t, ('b, 'd) Either.t) Choice.t
       )
       Law.t

  val choice_8 :
       unit
    -> ( ('a, 'b) Choice.t
       , ( ('c, ('d, 'a) Either.t) Either.t
         , ('c, ('d, 'b) Either.t) Either.t )
         Choice.t )
       Law.t
end

module For (C : Preface_specs.CHOICE) : LAWS with module Choice := C = struct
  open Law
  include Profunctor.For (C)

  let choice_1 () =
    let lhs x = C.left x
    and rhs x = C.dimap Util.swap_either Util.swap_either (C.right x) in

    law ("left" =~ lhs) ("dimap swap swap % right" =~ rhs)
  ;;

  let choice_2 () =
    let lhs x = C.map_snd Either.left x
    and rhs x = C.contramap_fst Either.left (C.left x) in

    law
      ("map_snd Either.left" =~ lhs)
      ("contramap_fst Either.left % left" =~ rhs)
  ;;

  let choice_3 () =
    let lhs f x = C.contramap_fst (Util.Fun.Choice.right f) (C.left x)
    and rhs f x = C.map_snd (Util.Fun.Choice.right f) (C.left x) in

    law
      ("contramap_fst (Fun.Choice.right f) % left" =~ lhs)
      ("map_snd (Fun.Choice.right f) % left" =~ rhs)
  ;;

  let choice_4 () =
    let lhs x = C.left (C.left x)
    and rhs x = C.dimap Util.assoc_either Util.unassoc_either (C.left x) in

    law ("left % left" =~ lhs) ("dimap assoc unassoc % left" =~ rhs)
  ;;

  let choice_5 () =
    let lhs x = C.right x
    and rhs x = C.dimap Util.swap_either Util.swap_either (C.left x) in

    law ("right" =~ lhs) ("dimap swap swap % left" =~ rhs)
  ;;

  let choice_6 () =
    let lhs x = C.map_snd Either.right x
    and rhs x = C.contramap_fst Either.right (C.right x) in

    law
      ("map_snd Either.right" =~ lhs)
      ("contramap_fst Either.right % right" =~ rhs)
  ;;

  let choice_7 () =
    let lhs f x = C.contramap_fst (Util.Fun.Choice.left f) (C.right x)
    and rhs f x = C.map_snd (Util.Fun.Choice.left f) (C.right x) in

    law
      ("contramap_fst (Fun.Choice.left f) % right" =~ lhs)
      ("map_snd (Fun.Choice.left f) % right" =~ rhs)
  ;;

  let choice_8 () =
    let lhs x = C.right (C.right x)
    and rhs x = C.dimap Util.unassoc_either Util.assoc_either (C.right x) in

    law ("right % right" =~ lhs) ("dimap unassoc assoc % left" =~ rhs)
  ;;
end
