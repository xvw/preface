(** Functor that generate a suite for a [Bounded_join_semilattice]. *)

module Suite
    (R : Model.COVARIANT_0)
    (L : Preface_specs.BOUNDED_JOIN_SEMILATTICE with type t = R.t) : Model.SUITE
