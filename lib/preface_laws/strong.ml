module type LAWS = sig
  type ('a, 'b) t

  val strong_1 : unit -> (('a, 'b) t, ('a * 'c, 'b * 'c) t) Law.t
  val strong_2 : unit -> (('a, 'b) t, ('a * 'c, 'b) t) Law.t
  val strong_3 : unit -> ('a -> 'b, ('c, 'd) t -> ('c * 'a, 'd * 'b) t) Law.t
  val strong_4 : unit -> (('a, 'b) t, (('a * 'c) * 'd, ('b * 'c) * 'd) t) Law.t
  val strong_5 : unit -> (('a, 'b) t, ('c * 'a, 'c * 'b) t) Law.t
  val strong_6 : unit -> (('a, 'b) t, ('c * 'a, 'b) t) Law.t
  val strong_7 : unit -> ('a -> 'b, ('c, 'd) t -> ('a * 'c, 'b * 'd) t) Law.t
  val strong_8 : unit -> (('a, 'b) t, ('c * ('d * 'a), 'c * ('d * 'b)) t) Law.t
end

module For (S : Preface_specs.STRONG) :
  LAWS with type ('a, 'b) t := ('a, 'b) S.t = struct
  open Law
  open Preface_core.Fun.Infix
  include Profunctor.For (S)

  let strong_1 () =
    let lhs x = S.fst x
    and rhs x = S.dimap Util.swap Util.swap (S.snd x) in

    law ("fst" =~ lhs) ("dimap swap swap % snd" =~ rhs)
  ;;

  let strong_2 () =
    let lhs x = S.contramap_fst fst x
    and rhs x = (S.map_snd fst % S.fst) x in

    law
      ("contramap_fst (fun (x, _) -> x)" =~ lhs)
      ("map_snd (fun (x, _) -> x) % fst" =~ rhs)
  ;;

  let strong_3 () =
    let lhs f x = (S.contramap_fst (Util.Fun.Strong.snd f) % S.fst) x
    and rhs f x = (S.map_snd (Util.Fun.Strong.snd f) % S.fst) x in

    law
      ("contramap_fst (Fun.Strong.snd f) % fst" =~ lhs)
      ("map_snd (Fun.Strong.snd f) % fst" =~ rhs)
  ;;

  let strong_4 () =
    let lhs x = (S.fst % S.fst) x
    and rhs x = Util.(S.dimap assoc unassoc % S.fst) x in

    law ("fst % fst" =~ lhs) ("dimap assoc unassoc % fst" =~ rhs)
  ;;

  let strong_5 () =
    let lhs x = S.snd x
    and rhs x = S.dimap Util.swap Util.swap (S.fst x) in

    law ("snd" =~ lhs) ("dimap swap swap % fst" =~ rhs)
  ;;

  let strong_6 () =
    let lhs x = S.contramap_fst snd x
    and rhs x = (S.map_snd snd % S.snd) x in

    law
      ("contramap_fst (fun (_, x) -> x)" =~ lhs)
      ("map_snd (fun (_, x) -> x) % snd" =~ rhs)
  ;;

  let strong_7 () =
    let lhs f x = (S.contramap_fst (Util.Fun.Strong.fst f) % S.snd) x
    and rhs f x = (S.map_snd (Util.Fun.Strong.fst f) % S.snd) x in

    law
      ("contramap_fst (Fun.Strong.fst f) % snd" =~ lhs)
      ("map_snd (Fun.Strong.fst f) % snd" =~ rhs)
  ;;

  let strong_8 () =
    let lhs x = (S.snd % S.snd) x
    and rhs x = Util.(S.dimap unassoc assoc % S.snd) x in

    law ("snd % snd" =~ lhs) ("dimap unassoc assoc  % snd" =~ rhs)
  ;;
end
