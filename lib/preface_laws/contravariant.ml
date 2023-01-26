module type LAWS = sig
  type 'a t

  val contravariant_1 : unit -> ('a t, 'a t) Law.t

  val contravariant_2 :
       unit
    -> ('a -> 'b, ('b -> 'c) -> 'c t -> 'a t) Law.t
end

module For (C : Preface_specs.CONTRAVARIANT) :
  LAWS with type 'a t := 'a C.t = struct
  open Law

  let contravariant_1 () =
    let lhs x = C.contramap Fun.id x
    and rhs x = x in

    law ("contramap id" =~ lhs) ("id" =~ rhs)
  ;;

  let contravariant_2 () =
    let open Preface_core.Fun.Infix in
    let lhs f g = C.contramap (g % f)
    and rhs f g = C.(contramap f % contramap g) in

    law ("contramap (g % f)" =~ lhs) ("(contramap f) % (contramap g)" =~ rhs)
  ;;
end
