module type LAWS = sig
  include Bounded_join_semilattice.LAWS
  include Bounded_meet_semilattice.LAWS with type t := t

  val bounded_lattice_1 : unit -> (t, t -> t) Law.t
  val bounded_lattice_2 : unit -> (t, t -> t) Law.t
end

module For (L : Preface_specs.BOUNDED_LATTICE) : LAWS with type t := L.t =
struct
  open Law
  include Bounded_join_semilattice.For (L)
  include Bounded_meet_semilattice.For (L)

  let bounded_lattice_1 () =
    let lhs a b = L.meet a (L.join a b)
    and rhs a _ = a in
    law ("meet a (join a b)" =~ lhs) ("a" =~ rhs)
  ;;

  let bounded_lattice_2 () =
    let lhs a b = L.join a (L.meet a b)
    and rhs a _ = a in
    law ("join a (meet a b)" =~ lhs) ("a" =~ rhs)
  ;;
end
