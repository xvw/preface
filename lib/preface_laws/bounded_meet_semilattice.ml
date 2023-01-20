module type LAWS = sig
  module Bounded_meet_semilattice : Preface_specs.BOUNDED_MEET_SEMILATTICE

  include
    Meet_semilattice.LAWS
      with module Meet_semilattice := Bounded_meet_semilattice

  val bounded_meet_semilattice_1 :
    unit -> (Bounded_meet_semilattice.t, Bounded_meet_semilattice.t) Law.t
end

module For (L : Preface_specs.BOUNDED_MEET_SEMILATTICE) :
  LAWS with module Bounded_meet_semilattice := L = struct
  open Law
  include Meet_semilattice.For (L)

  let bounded_meet_semilattice_1 () =
    let lhs x = L.meet x L.top
    and rhs x = x in
    law ("meet x top" =~ lhs) ("x" =~ rhs)
  ;;
end
