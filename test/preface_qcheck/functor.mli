(** Generate a suite for [Preface_specs.FUNCTOR]. *)

module Make
    (F : Preface_specs.FUNCTOR)
    (R : Requirement.INPUT_T1 with type 'a t = 'a F.t)
    (P : Sample.PACK) : Requirement.OUTPUT

(** Make a suite using a hook on each result (useful for Stream and
    Continuation). *)

module Make_hooked
    (F : Preface_specs.FUNCTOR)
    (R : Requirement.INPUT_T1 with type 'a t = 'a F.t)
    (Hook : Requirement.HOOK with type 'a t = 'a F.t)
    (P : Sample.PACK) : Requirement.OUTPUT
