module type LAWS = sig
  module Bind : Preface_specs.BIND
  include Functor.LAWS with module Functor := Bind

  val bind_1 : unit -> ('a Bind.t Bind.t Bind.t, 'a Bind.t) Law.t
  val bind_2 : unit -> ('a -> 'b, 'a Bind.t Bind.t -> 'b Bind.t) Law.t

  val bind_3 :
       unit
    -> ('a Bind.t, ('a -> 'b Bind.t) -> ('b -> 'c Bind.t) -> 'c Bind.t) Law.t

  val bind_4 :
       unit
    -> ( 'a -> 'b Bind.t
       , ('b -> 'c Bind.t) -> ('c -> 'd Bind.t) -> 'a -> 'd Bind.t )
       Law.t
end

module For (B : Preface_specs.BIND) : LAWS with module Bind := B = struct
  open Law
  open Preface_core.Fun.Infix
  include Functor.For (B)

  let bind_1 () =
    let lhs x = B.(join % join) x
    and rhs x = B.(join % map join) x in

    law ~lhs:("join % join" =~ lhs) ~rhs:("join % map join" =~ rhs)
  ;;

  let bind_2 () =
    let lhs f x = B.(map f % join) x
    and rhs f x = B.(join % map (map f)) x in

    law ~lhs:("map f % join" =~ lhs) ~rhs:("join % map (map f)" =~ rhs)
  ;;

  let bind_3 () =
    let lhs x f g = B.(x >>= Infix.(fun y -> f y >>= g))
    and rhs x f g = B.(Infix.(x >>= f) >>= g) in

    law
      ~lhs:("x >>= (fun y -> f y >>= g)" =~ lhs)
      ~rhs:("(x >>= f) >>= g" =~ rhs)
  ;;

  let bind_4 () =
    let lhs f g h x = B.(Infix.(f >=> g) >=> h) x
    and rhs f g h x = B.(f >=> Infix.(g >=> h)) x in

    law ~lhs:("(f >=> g) >=> h" =~ lhs) ~rhs:("f >=> (g >=> h)" =~ rhs)
  ;;
end
