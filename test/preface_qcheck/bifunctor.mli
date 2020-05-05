(** Generate a suite for [Preface_specs.BIFUNCTOR]. *)

module Make
    (B : Preface_specs.BIFUNCTOR)
    (R : Requirement.INPUT_T2 with type ('a, 'b) t = ('a, 'b) B.t)
    (P : Sample.PACK) : Requirement.OUTPUT
