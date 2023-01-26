module type LAWS = sig
  type t

  val bounded_meet_semilattice_1 :
    unit -> (t, t) Law.t
end

module For (L : Preface_specs.BOUNDED_MEET_SEMILATTICE) :
  LAWS with type t := L.t = struct
  open Law
  include Meet_semilattice.For (L)

  let bounded_meet_semilattice_1 () =
    let lhs x = L.meet x L.top
    and rhs x = x in
    law ("meet x top" =~ lhs) ("x" =~ rhs)
  ;;
end
