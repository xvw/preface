module type LAWS = sig
  module Contravariant : Preface_specs.CONTRAVARIANT

  val contravariant_preserve_identity_morphisms :
    unit -> ('a Contravariant.t, 'a Contravariant.t) Law.t

  val contravariant_preserve_composition_of_morphisms :
       unit
    -> ('a -> 'b, ('b -> 'c) -> 'c Contravariant.t -> 'a Contravariant.t) Law.t
end

module For (C : Preface_specs.CONTRAVARIANT) :
  LAWS with module Contravariant := C = struct
  open Law

  let contravariant_preserve_identity_morphisms () =
    let lhs x = C.contramap Fun.id x
    and rhs x = x in

    law "Preserve identity morphisms" ~lhs:("contramap id" =~ lhs)
      ~rhs:("id" =~ rhs)
  ;;

  let contravariant_preserve_composition_of_morphisms () =
    let open Preface_core.Fun.Infix in
    let lhs f g = C.contramap (g % f)
    and rhs f g = C.(contramap f % contramap g) in

    law "Preserve composition of morphisms"
      ~lhs:("contramap (g % f)" =~ lhs)
      ~rhs:("(contramap f) % (contramap g)" =~ rhs)
  ;;
end
