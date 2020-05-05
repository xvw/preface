(** Generate a suite for [Preface_specs.APPLICATIVE]. *)

module Make
    (A : Preface_specs.APPLICATIVE)
    (R : Requirement.INPUT_T1 with type 'a t = 'a A.t)
    (P : Sample.PACK) : Requirement.OUTPUT

(** Make a suite using a hook on each result (useful for Stream and
    Continuation). *)

module Make_hooked
    (A : Preface_specs.APPLICATIVE)
    (R : Requirement.INPUT_T1 with type 'a t = 'a A.t)
    (Hook : Requirement.HOOK with type 'a t = 'a A.t)
    (P : Sample.PACK) : Requirement.OUTPUT
