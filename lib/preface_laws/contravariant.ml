module type LAWS = sig
  module Contravariant : Preface_specs.CONTRAVARIANT

  val contravariant_1 : unit -> ('a Contravariant.t, 'a Contravariant.t) Law.t

  val contravariant_2 :
       unit
    -> ('a -> 'b, ('b -> 'c) -> 'c Contravariant.t -> 'a Contravariant.t) Law.t
end

module For (C : Preface_specs.CONTRAVARIANT) :
  LAWS with module Contravariant := C = struct
  open Law

  let contravariant_1 () =
    let lhs x = C.contramap Fun.id x
    and rhs x = x in

    law ~lhs:("contramap id" =~ lhs) ~rhs:("id" =~ rhs)
  ;;

  let contravariant_2 () =
    let open Preface_core.Fun.Infix in
    let lhs f g = C.contramap (g % f)
    and rhs f g = C.(contramap f % contramap g) in

    law
      ~lhs:("contramap (g % f)" =~ lhs)
      ~rhs:("(contramap f) % (contramap g)" =~ rhs)
  ;;
end
