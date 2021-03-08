open Aliases

module type LAWS = sig
  type ('a, 'b) t

  val dimap_identity : string * (unit -> (('a, 'b) t -> ('a, 'b) t) pair)

  val contramap_fst_identity : string * (unit -> (('a, 'b) t -> ('a, 'b) t) pair)

  val map_snd_identity : string * (unit -> (('a, 'b) t -> ('a, 'b) t) pair)

  val dimap_equality :
    string * (('a -> 'b) -> ('c -> 'd) -> (('b, 'c) t -> ('a, 'd) t) pair)

  val dimap_parametricity :
    string
    * (   ('a -> 'b)
       -> ('c -> 'a)
       -> ('d -> 'e)
       -> ('f -> 'd)
       -> (('b, 'f) t -> ('c, 'e) t) pair )

  val contramap_fst_parametricity :
    string * (('a -> 'b) -> ('c -> 'a) -> (('b, 'd) t -> ('c, 'd) t) pair)

  val map_snd_parametricity :
    string * (('a -> 'b) -> ('c -> 'a) -> (('d, 'c) t -> ('d, 'b) t) pair)
end

module Laws (P : Preface_specs.PROFUNCTOR) = struct
  type ('a, 'b) t = ('a, 'b) P.t

  let dimap_identity =
    let lhs () = P.dimap Fun.id Fun.id in
    let rhs () = Fun.id in
    ("dimap id id = id", (fun () -> (lhs (), rhs ())))
  ;;

  let contramap_fst_identity =
    let lhs () = P.contramap_fst Fun.id in
    let rhs () = Fun.id in
    ("contramap_fst id = id", (fun () -> (lhs (), rhs ())))
  ;;

  let map_snd_identity =
    let lhs () = P.map_snd Fun.id in
    let rhs () = Fun.id in
    ("map_snd id = id", (fun () -> (lhs (), rhs ())))
  ;;

  let dimap_equality =
    let open Preface_core.Fun.Infix in
    let lhs f g = P.dimap f g in
    let rhs f g = P.contramap_fst f % P.map_snd g in
    ("dimap f g = contramap_fst f % map_snd g", (fun f g -> (lhs f g, rhs f g)))
  ;;

  let dimap_parametricity =
    let open Preface_core.Fun.Infix in
    let lhs f g h i = P.dimap (f % g) (h % i) in
    let rhs f g h i = P.dimap g h % P.dimap f i in
    ( "dimap (f % g) (h % i) = dimap g h % dimap f i"
    , (fun f g h i -> (lhs f g h i, rhs f g h i)) )
  ;;

  let contramap_fst_parametricity =
    let open Preface_core.Fun.Infix in
    let lhs f g = P.contramap_fst (f % g) in
    let rhs f g = P.contramap_fst g % P.contramap_fst f in
    ( "contramap_fst (f % g) = contramap_fst g % contramap_fst f"
    , (fun f g -> (lhs f g, rhs f g)) )
  ;;

  let map_snd_parametricity =
    let open Preface_core.Fun.Infix in
    let lhs f g = P.map_snd (f % g) in
    let rhs f g = P.map_snd f % P.map_snd g in
    ("map_snd (f % g) = map_snd f % map_snd g", (fun f g -> (lhs f g, rhs f g)))
  ;;
end
