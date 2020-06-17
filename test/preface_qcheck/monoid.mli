(** Generate a suite for [Preface_specs.MONOID]. *)

module Make
    (M : Preface_specs.MONOID)
    (R : Requirement.INPUT_T with type t = M.t) : Requirement.OUTPUT
