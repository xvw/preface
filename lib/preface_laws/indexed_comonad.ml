module type LAWS = sig
  type ('a, 'index) t

  include Indexed_functor.LAWS with type ('a, 'index) t := ('a, 'index) t

  val comonad_1 : unit -> (('a, 'index) t, ('a, 'index) t) Law.t
  val comonad_2 : unit -> (('a, 'index) t -> 'b, ('a, 'index) t -> 'b) Law.t

  val comonad_3 :
       unit
    -> ( ('a, 'index) t -> 'b
       , (('c, 'index) t -> 'a) -> ('c, 'index) t -> ('b, 'index) t )
       Law.t

  val comonad_4 : unit -> (('a, 'index) t -> 'b, ('a, 'index) t -> 'b) Law.t
  val comonad_5 : unit -> (('a, 'index) t -> 'b, ('a, 'index) t -> 'b) Law.t

  val comonad_6 :
       unit
    -> ( ('a, 'index) t -> 'b
       ,    (('b, 'index) t -> 'c)
         -> (('c, 'index) t -> 'd)
         -> ('a, 'index) t
         -> 'd )
       Law.t

  val comonad_7 : unit -> (('a, 'index) t, ('a, 'index) t) Law.t
  val comonad_8 : unit -> (('a, 'index) t, ('a, 'index) t) Law.t

  val comonad_9 :
    unit -> (('a, 'index) t, ((('a, 'index) t, 'index) t, 'index) t) Law.t

  val comonad_10 :
    unit -> (('a, 'index) t -> 'b, ('a, 'index) t -> ('b, 'index) t) Law.t

  val comonad_11 : unit -> (('a, 'index) t, (('a, 'index) t, 'index) t) Law.t
  val comonad_12 : unit -> ('a -> 'b, ('a, 'index) t -> ('b, 'index) t) Law.t
end

module For (C : Preface_specs.INDEXED_COMONAD) :
  LAWS with type ('a, 'index) t := ('a, 'index) C.t = struct
  include Indexed_functor.For (C)
  open Preface_core.Fun
  open Law

  let comonad_1 () =
    let lhs x = C.(extend extract) x
    and rhs x = x in

    law ("extend extract" =~ lhs) ("id" =~ rhs)
  ;;

  let comonad_2 () =
    let lhs f x = C.(extract % extend f) x
    and rhs f x = f x in

    law ("extract % extend" =~ lhs) ("f" =~ rhs)
  ;;

  let comonad_3 () =
    let lhs f g x = C.(extend f % extend g) x
    and rhs f g x = C.(extend (f % extend g)) x in

    law ("extend f % extend g" =~ lhs) ("extend (f % extend g)" =~ rhs)
  ;;

  let comonad_4 () =
    let lhs f x = C.(f =>= extract) x
    and rhs f x = f x in

    law ("f =>= extract" =~ lhs) ("f" =~ rhs)
  ;;

  let comonad_5 () =
    let lhs f x = C.(extract =>= f) x
    and rhs f x = f x in

    law ("extract =>= f" =~ lhs) ("f" =~ rhs)
  ;;

  let comonad_6 () =
    let lhs f g h x = C.(Infix.(f =>= g) =>= h) x
    and rhs f g h x = C.(f =>= Infix.(g =>= h)) x in

    law ("(f =>= g) =>= h" =~ lhs) ("f =>= (g =>= h)" =~ rhs)
  ;;

  let comonad_7 () =
    let lhs x = C.(extract % duplicate) x
    and rhs x = x in

    law ("extract % duplicate" =~ lhs) ("id" =~ rhs)
  ;;

  let comonad_8 () =
    let lhs x = C.(map extract % duplicate) x
    and rhs x = x in

    law ("map extract % duplicate" =~ lhs) ("id" =~ rhs)
  ;;

  let comonad_9 () =
    let lhs x = C.(duplicate % duplicate) x
    and rhs x = C.(map duplicate % duplicate) x in

    law ("duplicate % duplicate" =~ lhs) ("map duplicate % duplicate" =~ rhs)
  ;;

  let comonad_10 () =
    let lhs f x = C.(extend f) x
    and rhs f x = C.(map f % duplicate) x in

    law ("extend f" =~ lhs) ("map f % duplicate" =~ rhs)
  ;;

  let comonad_11 () =
    let lhs x = C.duplicate x
    and rhs x = C.(extend (fun x -> x)) x in

    law ("duplicate" =~ lhs) ("extend id" =~ rhs)
  ;;

  let comonad_12 () =
    let lhs f x = C.map f x
    and rhs f x = C.(extend (f % extract)) x in

    law ("map f" =~ lhs) ("extend (f % extract)" =~ rhs)
  ;;
end
