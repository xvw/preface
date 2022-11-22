module type LAWS = sig
  module Functor : Preface_specs.FUNCTOR

  val functor_1 : unit -> ('a Functor.t, 'a Functor.t) Law.t

  val functor_2 :
    unit -> ('a -> 'b, ('c -> 'a) -> 'c Functor.t -> 'b Functor.t) Law.t
end

module For (F : Preface_specs.FUNCTOR) : LAWS with module Functor := F = struct
  open Law

  let functor_1 () =
    let lhs x = F.map Fun.id x
    and rhs x = x in

    law ~lhs:("map id" =~ lhs) ~rhs:("id" =~ rhs)
  ;;

  let functor_2 () =
    let open Preface_core.Fun.Infix in
    let lhs f g = F.map (f % g)
    and rhs f g = F.(map f % map g) in

    law ~lhs:("map (f % g)" =~ lhs) ~rhs:("(map f) % (map g)" =~ rhs)
  ;;
end
