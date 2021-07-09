module type LAWS = sig
  module Functor : Preface_specs.FUNCTOR

  val functor_preserve_identity_morphisms :
    unit -> ('a Functor.t, 'a Functor.t) Law.t

  val functor_preserve_composition_of_morphisms :
    unit -> ('a -> 'b, ('c -> 'a) -> 'c Functor.t -> 'b Functor.t) Law.t
end

module For (F : Preface_specs.FUNCTOR) : LAWS with module Functor := F = struct
  open Law

  let functor_preserve_identity_morphisms () =
    let lhs x = F.map Fun.id x
    and rhs x = x in

    law "Preserve identity morphisms" ~lhs:("map id" =~ lhs) ~rhs:("id" =~ rhs)
  ;;

  let functor_preserve_composition_of_morphisms () =
    let open Preface_core.Fun.Infix in
    let lhs f g = F.map (f % g)
    and rhs f g = F.(map f % map g) in

    law "Preserve composition of morphisms" ~lhs:("map (f % g)" =~ lhs)
      ~rhs:("(map f) % (map g)" =~ rhs)
  ;;
end
