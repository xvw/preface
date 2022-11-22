module type LAWS = sig
  module Strong : Preface_specs.STRONG
  include Profunctor.LAWS with module Profunctor := Strong

  val strong_1 : unit -> (('a, 'b) Strong.t, ('a * 'c, 'b * 'c) Strong.t) Law.t
  val strong_2 : unit -> (('a, 'b) Strong.t, ('a * 'c, 'b) Strong.t) Law.t

  val strong_3 :
    unit -> ('a -> 'b, ('c, 'd) Strong.t -> ('c * 'a, 'd * 'b) Strong.t) Law.t

  val strong_4 :
    unit -> (('a, 'b) Strong.t, (('a * 'c) * 'd, ('b * 'c) * 'd) Strong.t) Law.t

  val strong_5 : unit -> (('a, 'b) Strong.t, ('c * 'a, 'c * 'b) Strong.t) Law.t
  val strong_6 : unit -> (('a, 'b) Strong.t, ('c * 'a, 'b) Strong.t) Law.t

  val strong_7 :
    unit -> ('a -> 'b, ('c, 'd) Strong.t -> ('a * 'c, 'b * 'd) Strong.t) Law.t

  val strong_8 :
    unit -> (('a, 'b) Strong.t, ('c * ('d * 'a), 'c * ('d * 'b)) Strong.t) Law.t
end

module For (S : Preface_specs.STRONG) : LAWS with module Strong := S = struct
  open Law
  open Preface_core.Fun.Infix
  include Profunctor.For (S)

  let strong_1 () =
    let lhs x = S.fst x
    and rhs x = S.dimap Util.swap Util.swap (S.snd x) in

    law ~lhs:("fst" =~ lhs) ~rhs:("dimap swap swap % snd" =~ rhs)
  ;;

  let strong_2 () =
    let lhs x = S.contramap_fst fst x
    and rhs x = (S.map_snd fst % S.fst) x in

    law
      ~lhs:("contramap_fst (fun (x, _) -> x)" =~ lhs)
      ~rhs:("map_snd (fun (x, _) -> x) % fst" =~ rhs)
  ;;

  let strong_3 () =
    let lhs f x = (S.contramap_fst (Util.Fun.Strong.snd f) % S.fst) x
    and rhs f x = (S.map_snd (Util.Fun.Strong.snd f) % S.fst) x in

    law
      ~lhs:("contramap_fst (Fun.Strong.snd f) % fst" =~ lhs)
      ~rhs:("map_snd (Fun.Strong.snd f) % fst" =~ rhs)
  ;;

  let strong_4 () =
    let lhs x = (S.fst % S.fst) x
    and rhs x = Util.(S.dimap assoc unassoc % S.fst) x in

    law ~lhs:("fst % fst" =~ lhs) ~rhs:("dimap assoc unassoc % fst" =~ rhs)
  ;;

  let strong_5 () =
    let lhs x = S.snd x
    and rhs x = S.dimap Util.swap Util.swap (S.fst x) in

    law ~lhs:("snd" =~ lhs) ~rhs:("dimap swap swap % fst" =~ rhs)
  ;;

  let strong_6 () =
    let lhs x = S.contramap_fst snd x
    and rhs x = (S.map_snd snd % S.snd) x in

    law
      ~lhs:("contramap_fst (fun (_, x) -> x)" =~ lhs)
      ~rhs:("map_snd (fun (_, x) -> x) % snd" =~ rhs)
  ;;

  let strong_7 () =
    let lhs f x = (S.contramap_fst (Util.Fun.Strong.fst f) % S.snd) x
    and rhs f x = (S.map_snd (Util.Fun.Strong.fst f) % S.snd) x in

    law
      ~lhs:("contramap_fst (Fun.Strong.fst f) % snd" =~ lhs)
      ~rhs:("map_snd (Fun.Strong.fst f) % snd" =~ rhs)
  ;;

  let strong_8 () =
    let lhs x = (S.snd % S.snd) x
    and rhs x = Util.(S.dimap unassoc assoc % S.snd) x in

    law ~lhs:("snd % snd" =~ lhs) ~rhs:("dimap unassoc assoc  % snd" =~ rhs)
  ;;
end
