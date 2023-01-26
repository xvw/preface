module type LAWS = sig
  type 'a t

  val bind_1 : unit -> ('a t t t, 'a t) Law.t
  val bind_2 : unit -> ('a -> 'b, 'a t t -> 'b t) Law.t
  val bind_3 : unit -> ('a t, ('a -> 'b t) -> ('b -> 'c t) -> 'c t) Law.t

  val bind_4 :
    unit -> ('a -> 'b t, ('b -> 'c t) -> ('c -> 'd t) -> 'a -> 'd t) Law.t
end

module For (B : Preface_specs.BIND) : LAWS with type 'a t := 'a B.t = struct
  open Law
  open Preface_core.Fun.Infix
  include Functor.For (B)

  let bind_1 () =
    let lhs x = B.(join % join) x
    and rhs x = B.(join % map join) x in

    law ("join % join" =~ lhs) ("join % map join" =~ rhs)
  ;;

  let bind_2 () =
    let lhs f x = B.(map f % join) x
    and rhs f x = B.(join % map (map f)) x in

    law ("map f % join" =~ lhs) ("join % map (map f)" =~ rhs)
  ;;

  let bind_3 () =
    let lhs x f g = B.(x >>= Infix.(fun y -> f y >>= g))
    and rhs x f g = B.(Infix.(x >>= f) >>= g) in

    law ("x >>= (fun y -> f y >>= g)" =~ lhs) ("(x >>= f) >>= g" =~ rhs)
  ;;

  let bind_4 () =
    let lhs f g h x = B.(Infix.(f >=> g) >=> h) x
    and rhs f g h x = B.(f >=> Infix.(g >=> h)) x in

    law ("(f >=> g) >=> h" =~ lhs) ("f >=> (g >=> h)" =~ rhs)
  ;;
end
