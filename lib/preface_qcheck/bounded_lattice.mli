(** Functor that generate a suite for a [Bounded_lattice]. *)

module Suite
    (R : Model.COVARIANT_0)
    (L : Preface_specs.BOUNDED_LATTICE with type t = R.t) : Model.SUITE
