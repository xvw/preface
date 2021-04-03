open Aliases
module P = Preface_stdlib.Pair

module type LAWS = sig
  include Profunctor.LAWS

  val fst_define_snd : string * (('a, 'b) t -> ('a * 'c, 'b * 'c) t pair)

  val snd_define_fst : string * (('a, 'b) t -> ('c * 'a, 'c * 'b) t pair)

  val contramap_fst : string * (('a, 'b) t -> ('a * 'c, 'b) t pair)

  val contramap_snd : string * (('a, 'b) t -> ('c * 'a, 'b) t pair)

  val dinaturality_fst :
    string * (('a -> 'b) -> ('c, 'd) t -> ('c * 'a, 'd * 'b) t pair)

  val dinaturality_snd :
    string
    * (('a -> 'b) -> ('c, 'd) t -> ('a * 'c, 'b * 'd) t * ('a * 'c, 'b * 'd) t)

  val fst_fst_is_dmap :
    string * (('a, 'b) t -> (('a * 'c) * 'd, ('b * 'c) * 'd) t pair)

  val snd_snd_is_dmap :
    string
    * (   ('a * 'b, 'c * 'd) t
       -> ('e * ('f * ('a * 'b)), 'e * ('f * ('c * 'd))) t pair )
end

module Laws (S : Preface_specs.STRONG) = struct
  include Profunctor.Laws (S)

  let fst_define_snd =
    let lhs x = S.fst x in
    let rhs x = S.dimap P.swap P.swap (S.snd x) in
    ("fst = dimap Pair.swap Pair.swap % snd", (fun x -> (lhs x, rhs x)))
  ;;

  let snd_define_fst =
    let lhs x = S.snd x in
    let rhs x = S.dimap P.swap P.swap (S.fst x) in
    ("fst = dimap Pair.swap Pair.swap % snd", (fun x -> (lhs x, rhs x)))
  ;;

  let contramap_fst =
    let lhs x = S.contramap_fst P.fst x in
    let rhs x = S.map_snd P.fst (S.fst x) in
    ( "contramap_fst Pair.fst = (map_snd Pair.fst) % fst"
    , (fun x -> (lhs x, rhs x)) )
  ;;

  let contramap_snd =
    let lhs x = S.contramap_fst P.snd x in
    let rhs x = S.map_snd P.snd (S.snd x) in
    ( "contramap_fst Pair.snd = (map_snd Pair.snd) % snd"
    , (fun x -> (lhs x, rhs x)) )
  ;;

  let dinaturality_fst =
    let open Preface_core.Fun.Infix in
    let snd = Preface_stdlib.Fun.Strong.snd in
    let lhs f = S.contramap_fst (snd f) % S.fst in
    let rhs f = S.map_snd (snd f) % S.fst in
    ( "contramap_fst (Fun.Strong.snd f) % fst = map_snd (Fun.Strong.snd f) % fst"
    , (fun f x -> (lhs f x, rhs f x)) )
  ;;

  let dinaturality_snd =
    let open Preface_core.Fun.Infix in
    let fst = Preface_stdlib.Fun.Strong.fst in
    let lhs f = S.contramap_fst (fst f) % S.snd in
    let rhs f = S.map_snd (fst f) % S.snd in
    ( "contramap_fst (Fun.Strong.fst f) % snd = map_snd (Fun.Strong.fst f) % snd"
    , (fun f x -> (lhs f x, rhs f x)) )
  ;;

  let assoc ((a, b), c) = (a, (b, c))

  let unassoc (a, (b, c)) = ((a, b), c)

  let fst_fst_is_dmap =
    let open Preface_core.Fun.Infix in
    let lhs x = (S.fst % S.fst) x in
    let rhs x = (S.dimap assoc unassoc % S.fst) x in
    ( "fst % fst = dimap (fun ((a, b), c) ->  (a, (b, c))) (fun (a, (b, c)) -> \
       ((a, b), c)) % fst"
    , (fun x -> (lhs x, rhs x)) )
  ;;

  let snd_snd_is_dmap =
    let open Preface_core.Fun.Infix in
    let lhs x = (S.snd % S.snd) x in
    let rhs x = (S.dimap unassoc assoc % S.snd) x in
    ( "snd % snd = dimap (fun ((a, b), c) ->  (a, (b, c))) (fun (a, (b, c)) -> \
       ((a, b), c)) % snd"
    , (fun x -> (lhs x, rhs x)) )
  ;;
end
