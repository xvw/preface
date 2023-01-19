module type LAWS = sig
  module Join_semilattice : Preface_specs.JOIN_SEMILATTICE

  val join_semilattice_1 :
       unit
    -> ( Join_semilattice.t
       , Join_semilattice.t -> Join_semilattice.t -> Join_semilattice.t )
       Law.t

  val join_semilattice_2 :
    unit -> (Join_semilattice.t, Join_semilattice.t -> Join_semilattice.t) Law.t

  val join_semilattice_3 :
    unit -> (Join_semilattice.t, Join_semilattice.t) Law.t
end

module For (L : Preface_specs.JOIN_SEMILATTICE) :
  LAWS with module Join_semilattice := L = struct
  open Law

  let join_semilattice_1 () =
    let lhs x y z = L.join x (L.join y z)
    and rhs x y z = L.join (L.join x y) z in
    law ("join x (join y z)" =~ lhs) ("join (join x y) z" =~ rhs)
  ;;

  let join_semilattice_2 () =
    let lhs x y = L.join x y
    and rhs x y = L.join y x in
    law ("join x y" =~ lhs) ("join y x" =~ rhs)
  ;;

  let join_semilattice_3 () =
    let lhs x = L.join x x
    and rhs x = x in
    law ("join x x" =~ lhs) ("x" =~ rhs)
  ;;
end