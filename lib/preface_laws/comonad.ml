module type LAWS = sig
  module Comonad : Preface_specs.COMONAD
  include Functor.LAWS with module Functor := Comonad

  val comonad_extend_preserve_identity :
    unit -> ('a Comonad.t, 'a Comonad.t) Law.t

  val comonad_extract_extend :
    unit -> ('a Comonad.t -> 'b, 'a Comonad.t -> 'b) Law.t

  val comonad_extend_extend :
       unit
    -> ( 'a Comonad.t -> 'b
       , ('c Comonad.t -> 'a) -> 'c Comonad.t -> 'b Comonad.t )
       Law.t

  val comonad_left_extract_extend :
    unit -> ('a Comonad.t -> 'b, 'a Comonad.t -> 'b) Law.t

  val comonad_right_extract_extend :
    unit -> ('a Comonad.t -> 'b, 'a Comonad.t -> 'b) Law.t

  val comonad_extend_associative :
       unit
    -> ( 'a Comonad.t -> 'b
       , ('b Comonad.t -> 'c) -> ('c Comonad.t -> 'd) -> 'a Comonad.t -> 'd )
       Law.t

  val comonad_duplicate_preserve_identity :
    unit -> ('a Comonad.t, 'a Comonad.t) Law.t

  val comonad_map_duplicate_preserve_identity :
    unit -> ('a Comonad.t, 'a Comonad.t) Law.t

  val comonad_duplicate_duplicate_is_map_duplicate_duplicate :
    unit -> ('a Comonad.t, 'a Comonad.t Comonad.t Comonad.t) Law.t

  val comonad_extend_map_duplicate :
    unit -> ('a Comonad.t -> 'b, 'a Comonad.t -> 'b Comonad.t) Law.t

  val comonad_duplicate_extend_id :
    unit -> ('a Comonad.t, 'a Comonad.t Comonad.t) Law.t

  val comonad_map_extend_extract :
    unit -> ('a -> 'b, 'a Comonad.t -> 'b Comonad.t) Law.t
end

module For (C : Preface_specs.COMONAD) : LAWS with module Comonad := C = struct
  include Functor.For (C)
  open Preface_core.Fun
  open Law

  let comonad_extend_preserve_identity () =
    let lhs x = C.(extend extract) x
    and rhs x = x in

    law "Extend preserve identity" ~lhs:("extend extract" =~ lhs)
      ~rhs:("id" =~ rhs)
  ;;

  let comonad_extract_extend () =
    let lhs f x = C.(extract % extend f) x
    and rhs f x = f x in

    law "Extract and extend of f is f"
      ~lhs:("extract % extend" =~ lhs)
      ~rhs:("f" =~ rhs)
  ;;

  let comonad_extend_extend () =
    let lhs f g x = C.(extend f % extend g) x
    and rhs f g x = C.(extend (f % extend g)) x in

    law
      "A composition of extend f and g is and extend of a composition f and \
       extend g"
      ~lhs:("extend f % extend g" =~ lhs)
      ~rhs:("extend (f % extend g)" =~ rhs)
  ;;

  let comonad_left_extract_extend () =
    let lhs f x = C.(f =>= extract) x
    and rhs f x = f x in

    law "Infix version of extract extend" ~lhs:("f =>= extract" =~ lhs)
      ~rhs:("f" =~ rhs)
  ;;

  let comonad_right_extract_extend () =
    let lhs f x = C.(extract =>= f) x
    and rhs f x = f x in

    law "Right infix version of extract extend" ~lhs:("extract =>= f" =~ lhs)
      ~rhs:("f" =~ rhs)
  ;;

  let comonad_extend_associative () =
    let lhs f g h x = C.(Infix.(f =>= g) =>= h) x
    and rhs f g h x = C.(f =>= Infix.(g =>= h)) x in

    law "Extend is associative" ~lhs:("(f =>= g) =>= h" =~ lhs)
      ~rhs:("f =>= (g =>= h)" =~ rhs)
  ;;

  let comonad_duplicate_preserve_identity () =
    let lhs x = C.(extract % duplicate) x
    and rhs x = x in

    law "Duplicate preserve identity"
      ~lhs:("extract % duplicate" =~ lhs)
      ~rhs:("id" =~ rhs)
  ;;

  let comonad_map_duplicate_preserve_identity () =
    let lhs x = C.(map extract % duplicate) x
    and rhs x = x in

    law "Map over duplicate preserve identity"
      ~lhs:("map extract % duplicate" =~ lhs)
      ~rhs:("id" =~ rhs)
  ;;

  let comonad_duplicate_duplicate_is_map_duplicate_duplicate () =
    let lhs x = C.(duplicate % duplicate) x
    and rhs x = C.(map duplicate % duplicate) x in

    law "Duplicate over duplicate is map duplicate over duplicate"
      ~lhs:("duplicate % duplicate" =~ lhs)
      ~rhs:("map duplicate % duplicate" =~ rhs)
  ;;

  let comonad_extend_map_duplicate () =
    let lhs f x = C.(extend f) x
    and rhs f x = C.(map f % duplicate) x in
    law "Extend f is map f over duplicate" ~lhs:("extend f" =~ lhs)
      ~rhs:("map f % duplicate" =~ rhs)
  ;;

  let comonad_duplicate_extend_id () =
    let lhs x = C.duplicate x
    and rhs x = C.(extend (fun x -> x)) x in

    law "Duplicate is extend id" ~lhs:("duplicate" =~ lhs)
      ~rhs:("extend id" =~ rhs)
  ;;

  let comonad_map_extend_extract () =
    let lhs f x = C.map f x
    and rhs f x = C.(extend (f % extract)) x in

    law "map is extend over extract" ~lhs:("map f" =~ lhs)
      ~rhs:("extend (f % extract)" =~ rhs)
  ;;
end
