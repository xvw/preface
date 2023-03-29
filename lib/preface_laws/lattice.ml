module type LAWS = sig
  include Join_semilattice.LAWS
  include Meet_semilattice.LAWS with type t := t

  val lattice_1 : unit -> (t, t -> t) Law.t
  val lattice_2 : unit -> (t, t -> t) Law.t
end

module For (L : Preface_specs.LATTICE) : LAWS with type t := L.t = struct
  open Law
  include Join_semilattice.For (L)
  include Meet_semilattice.For (L)

  let lattice_1 () =
    let lhs a b = L.meet a (L.join a b)
    and rhs a _ = a in
    law ("meet a (join a b)" =~ lhs) ("a" =~ rhs)
  ;;

  let lattice_2 () =
    let lhs a b = L.join a (L.meet a b)
    and rhs a _ = a in
    law ("join a (meet a b)" =~ lhs) ("a" =~ rhs)
  ;;
end
