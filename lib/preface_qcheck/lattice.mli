(** Functor that generate a suite for a [Lattice]. *)

module Suite
    (R : Model.COVARIANT_0)
    (L : Preface_specs.LATTICE with type t = R.t) : Model.SUITE
