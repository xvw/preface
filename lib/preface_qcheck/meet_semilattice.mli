(** Functor that generate a suite for a [Meet_semilattice]. *)

module Suite
    (R : Model.COVARIANT_0)
    (L : Preface_specs.MEET_SEMILATTICE with type t = R.t) : Model.SUITE
