module type LAWS = sig
  module Bifunctor : Preface_specs.BIFUNCTOR

  val bifunctor_bimap_identity :
    unit -> (('a, 'b) Bifunctor.t, ('a, 'b) Bifunctor.t) Law.t

  val bifunctor_map_fst_identity :
    unit -> (('a, 'b) Bifunctor.t, ('a, 'b) Bifunctor.t) Law.t

  val bifunctor_map_snd_identity :
    unit -> (('a, 'b) Bifunctor.t, ('a, 'b) Bifunctor.t) Law.t

  val bifunctor_fst_snd :
       unit
    -> ( 'a -> 'b
       , ('c -> 'd) -> ('a, 'c) Bifunctor.t -> ('b, 'd) Bifunctor.t )
       Law.t

  val bifunctor_bimap_parametrecity :
       unit
    -> ( 'a -> 'b
       ,    ('c -> 'a)
         -> ('d -> 'e)
         -> ('f -> 'd)
         -> ('c, 'f) Bifunctor.t
         -> ('b, 'e) Bifunctor.t )
       Law.t

  val bifunctor_map_fst_parametrecity :
       unit
    -> ( 'a -> 'b
       , ('c -> 'a) -> ('c, 'd) Bifunctor.t -> ('b, 'd) Bifunctor.t )
       Law.t

  val bifunctor_map_snd_parametrecity :
       unit
    -> ( 'a -> 'b
       , ('c -> 'a) -> ('d, 'c) Bifunctor.t -> ('d, 'b) Bifunctor.t )
       Law.t
end

module For (B : Preface_specs.BIFUNCTOR) : LAWS with module Bifunctor := B =
struct
  open Law
  open Preface_core.Fun.Infix

  let bifunctor_bimap_identity () =
    let lhs x = B.bimap (fun x -> x) (fun x -> x) x
    and rhs x = x in

    law "identity" ~lhs:("bimap id id" =~ lhs) ~rhs:("id" =~ rhs)
  ;;

  let bifunctor_map_fst_identity () =
    let lhs x = B.map_fst (fun x -> x) x
    and rhs x = x in

    law "identity" ~lhs:("map_fst id" =~ lhs) ~rhs:("id" =~ rhs)
  ;;

  let bifunctor_map_snd_identity () =
    let lhs x = B.map_snd (fun x -> x) x
    and rhs x = x in

    law "identity" ~lhs:("map_snd id" =~ lhs) ~rhs:("id" =~ rhs)
  ;;

  let bifunctor_fst_snd () =
    let lhs f g x = B.bimap f g x
    and rhs f g x = (B.map_fst f % B.map_snd g) x in

    law "bimap is the composition of map_fst and map_snd"
      ~lhs:("bimap f g" =~ lhs)
      ~rhs:("map_fst f % map_snd g" =~ rhs)
  ;;

  let bifunctor_bimap_parametrecity () =
    let lhs f g h i x = B.bimap (f % g) (h % i) x
    and rhs f g h i x = (B.bimap f h % B.bimap g i) x in
    law "parametricity of bimap"
      ~lhs:("bimap (f % g) (h % i)" =~ lhs)
      ~rhs:("bimap f h % bimap g i" =~ rhs)
  ;;

  let bifunctor_map_fst_parametrecity () =
    let lhs f g x = B.map_fst (f % g) x
    and rhs f g x = (B.map_fst f % B.map_fst g) x in
    law "parametricity of map_fst" ~lhs:("map_fst (f % g)" =~ lhs)
      ~rhs:("map_fst f % map_fst g" =~ rhs)
  ;;

  let bifunctor_map_snd_parametrecity () =
    let lhs f g x = B.map_snd (f % g) x
    and rhs f g x = (B.map_snd f % B.map_snd g) x in
    law "parametricity of map_snd" ~lhs:("map_snd (f % g)" =~ lhs)
      ~rhs:("map_snd f % map_snd g" =~ rhs)
  ;;
end
