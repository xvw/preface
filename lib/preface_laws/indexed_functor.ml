module type LAWS = sig
  type ('a, 'index) t

  val functor_1 : unit -> (('a, 'index) t, ('a, 'index) t) Law.t

  val functor_2 :
    unit -> ('a -> 'b, ('c -> 'a) -> ('c, 'index) t -> ('b, 'index) t) Law.t
end

module For (F : Preface_specs.INDEXED_FUNCTOR) :
  LAWS with type ('a, 'index) t := ('a, 'index) F.t = struct
  open Law

  let functor_1 () =
    let lhs x = F.map Fun.id x
    and rhs x = x in

    law ("map id" =~ lhs) ("id" =~ rhs)
  ;;

  let functor_2 () =
    let open Preface_core.Fun.Infix in
    let lhs f g = F.map (f % g)
    and rhs f g = F.(map f % map g) in

    law ("map (f % g)" =~ lhs) ("(map f) % (map g)" =~ rhs)
  ;;
end
