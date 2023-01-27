module type LAWS = sig
  type t

  val bounded_join_semilattice_1 : unit -> (t, t) Law.t
end

module For (L : Preface_specs.BOUNDED_JOIN_SEMILATTICE) :
  LAWS with type t := L.t = struct
  open Law
  include Join_semilattice.For (L)

  let bounded_join_semilattice_1 () =
    let lhs x = L.join x L.bottom
    and rhs x = x in
    law ("join x bottom" =~ lhs) ("x" =~ rhs)
  ;;
end
