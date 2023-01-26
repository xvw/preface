(** Functor that generate a suite for a [Bounded_meet_semilattice]. *)

module Suite
    (R : Model.COVARIANT_0)
    (L : Preface_specs.BOUNDED_MEET_SEMILATTICE with type t = R.t) : Model.SUITE
