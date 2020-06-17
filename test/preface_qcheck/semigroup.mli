(** Generate a suite for [Preface_specs.SEMIGROUP]. *)

module Make
    (S : Preface_specs.SEMIGROUP)
    (R : Requirement.INPUT_T with type t = S.t) : Requirement.OUTPUT
