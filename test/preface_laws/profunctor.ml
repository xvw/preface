open Aliases

module type LAWS = sig
  type ('a, 'b) t

  val dimap_identity : string * (('a, 'b) t -> ('a, 'b) t pair)
  val contramap_fst_identity : string * (('a, 'b) t -> ('a, 'b) t pair)
  val map_snd_identity : string * (('a, 'b) t -> ('a, 'b) t pair)

  val dimap_equality :
    string * (('a -> 'b) -> ('c -> 'd) -> ('b, 'c) t -> ('a, 'd) t pair)

  val dimap_parametricity :
    string
    * (   ('a -> 'b)
       -> ('c -> 'a)
       -> ('d -> 'e)
       -> ('f -> 'd)
       -> ('b, 'f) t
       -> ('c, 'e) t pair )

  val contramap_fst_parametricity :
    string * (('a -> 'b) -> ('c -> 'a) -> ('b, 'd) t -> ('c, 'd) t pair)

  val map_snd_parametricity :
    string * (('a -> 'b) -> ('c -> 'a) -> ('d, 'c) t -> ('d, 'b) t pair)
end

module Laws (P : Preface_specs.PROFUNCTOR) = struct
  type ('a, 'b) t = ('a, 'b) P.t

  let dimap_identity =
    let lhs x = P.dimap Fun.id Fun.id x in
    let rhs x = Fun.id x in
    ("dimap id id = id", fun x -> (lhs x, rhs x))
  ;;

  let contramap_fst_identity =
    let lhs x = P.contramap_fst Fun.id x in
    let rhs x = Fun.id x in
    ("contramap_fst id = id", fun x -> (lhs x, rhs x))
  ;;

  let map_snd_identity =
    let lhs x = P.map_snd Fun.id x in
    let rhs x = Fun.id x in
    ("map_snd id = id", fun x -> (lhs x, rhs x))
  ;;

  let dimap_equality =
    let open Preface_core.Fun.Infix in
    let lhs f g x = P.dimap f g x in
    let rhs f g x = (P.contramap_fst f % P.map_snd g) x in
    ( "dimap f g = contramap_fst f % map_snd g"
    , fun f g x -> (lhs f g x, rhs f g x) )
  ;;

  let dimap_parametricity =
    let open Preface_core.Fun.Infix in
    let lhs f g h i x = P.dimap (f % g) (h % i) x in
    let rhs f g h i x = (P.dimap g h % P.dimap f i) x in
    ( "dimap (f % g) (h % i) = dimap g h % dimap f i"
    , fun f g h i x -> (lhs f g h i x, rhs f g h i x) )
  ;;

  let contramap_fst_parametricity =
    let open Preface_core.Fun.Infix in
    let lhs f g x = P.contramap_fst (f % g) x in
    let rhs f g x = (P.contramap_fst g % P.contramap_fst f) x in
    ( "contramap_fst (f % g) = contramap_fst g % contramap_fst f"
    , fun f g x -> (lhs f g x, rhs f g x) )
  ;;

  let map_snd_parametricity =
    let open Preface_core.Fun.Infix in
    let lhs f g x = P.map_snd (f % g) x in
    let rhs f g x = (P.map_snd f % P.map_snd g) x in
    ( "map_snd (f % g) = map_snd f % map_snd g"
    , fun f g x -> (lhs f g x, rhs f g x) )
  ;;
end
