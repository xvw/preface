module type LAWS = sig
  module Meet_semilattice : Preface_specs.MEET_SEMILATTICE

  val meet_semilattice_1 :
       unit
    -> ( Meet_semilattice.t
       , Meet_semilattice.t -> Meet_semilattice.t -> Meet_semilattice.t )
       Law.t

  val meet_semilattice_2 :
    unit -> (Meet_semilattice.t, Meet_semilattice.t -> Meet_semilattice.t) Law.t

  val meet_semilattice_3 :
    unit -> (Meet_semilattice.t, Meet_semilattice.t) Law.t
end

module For (L : Preface_specs.MEET_SEMILATTICE) :
  LAWS with module Meet_semilattice := L = struct
  open Law

  let meet_semilattice_1 () =
    let lhs x y z = L.meet x (L.meet y z)
    and rhs x y z = L.meet (L.meet x y) z in
    law ("meet x (meet y z)" =~ lhs) ("meet (meet x y) z" =~ rhs)
  ;;

  let meet_semilattice_2 () =
    let lhs x y = L.meet x y
    and rhs x y = L.meet y x in
    law ("meet x y" =~ lhs) ("meet y x" =~ rhs)
  ;;

  let meet_semilattice_3 () =
    let lhs x = L.meet x x
    and rhs x = x in
    law ("meet x x" =~ lhs) ("x" =~ rhs)
  ;;
end
