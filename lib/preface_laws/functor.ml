module type LAWS = sig
  module Functor : Preface_specs.FUNCTOR

  val preserve_identity_morphisms : unit -> ('a Functor.t, 'a Functor.t) Law.t

  val preserve_composition_of_morphisms :
    unit -> ('a -> 'b, ('c -> 'a) -> 'c Functor.t -> 'b Functor.t) Law.t
end

module For (F : Preface_specs.FUNCTOR) = struct
  open Law

  let preserve_identity_morphisms () =
    make "Preserve identity morphisms"
      (Side.make "map id" (F.map Fun.id))
      (Side.make "id" Fun.id)
  ;;

  let preserve_composition_of_morphisms () =
    let open Preface_core.Fun in
    make "Preserve composition of morphisms"
      (Side.make "map (f % g)" (fun f g -> F.map (f % g)))
      (Side.make "(map f) % (map g)" (fun f g -> F.(map f % map g)))
  ;;
end
