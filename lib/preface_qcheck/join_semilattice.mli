(** Functor that generate a suite for a [Join_semilattice]. *)

module Suite
    (R : Model.COVARIANT_0)
    (L : Preface_specs.JOIN_SEMILATTICE with type t = R.t) : Model.SUITE
