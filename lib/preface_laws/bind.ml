module type LAWS = sig
  module Bind : Preface_specs.BIND
  include Functor.LAWS with module Functor := Bind

  val bind_join_map : unit -> ('a Bind.t Bind.t Bind.t, 'a Bind.t) Law.t

  val bind_natural_transformation_map_join :
    unit -> ('a -> 'b, 'a Bind.t Bind.t -> 'b Bind.t) Law.t

  val bind_associativity_of_bind :
       unit
    -> ('a Bind.t, ('a -> 'b Bind.t) -> ('b -> 'c Bind.t) -> 'c Bind.t) Law.t

  val bind_associativity_of_kleisli_composition :
       unit
    -> ( 'a -> 'b Bind.t
       , ('b -> 'c Bind.t) -> ('c -> 'd Bind.t) -> 'a -> 'd Bind.t )
       Law.t
end

module For (B : Preface_specs.BIND) : LAWS with module Bind := B = struct
  open Law
  open Preface_core.Fun.Infix
  include Functor.For (B)

  let bind_join_map () =
    let lhs x = B.(join % join) x
    and rhs x = B.(join % map join) x in

    law "Join % Join is Join % Map Join" ~lhs:("join % join" =~ lhs)
      ~rhs:("join % map join" =~ rhs)
  ;;

  let bind_natural_transformation_map_join () =
    let lhs f x = B.(map f % join) x
    and rhs f x = B.(join % map (map f)) x in

    law "Map and join form a natural transformation" ~lhs:("map f % join" =~ lhs)
      ~rhs:("join % map (map f)" =~ rhs)
  ;;

  let bind_associativity_of_bind () =
    let lhs x f g = B.(x >>= Infix.(fun y -> f y >>= g))
    and rhs x f g = B.(Infix.(x >>= f) >>= g) in

    law "(>>=) is associative"
      ~lhs:("x >>= (fun y -> f y >>= g)" =~ lhs)
      ~rhs:("(x >>= f) >>= g" =~ rhs)
  ;;

  let bind_associativity_of_kleisli_composition () =
    let lhs f g h x = B.(Infix.(f >=> g) >=> h) x
    and rhs f g h x = B.(f >=> Infix.(g >=> h)) x in

    law "(>=>) is associative" ~lhs:("(f >=> g) >=> h" =~ lhs)
      ~rhs:("f >=> (g >=> h)" =~ rhs)
  ;;
end
