(** Generate a suite for [Preface_specs.MONAD]. *)

module Make
    (M : Preface_specs.MONAD)
    (R : Requirement.INPUT_T1 with type 'a t = 'a M.t)
    (P : Sample.PACK) : Requirement.OUTPUT

(** Make a suite using a hook on each result (useful for Stream and
    Continuation). *)

module Make_hooked
    (M : Preface_specs.MONAD)
    (R : Requirement.INPUT_T1 with type 'a t = 'a M.t)
    (Hook : Requirement.HOOK with type 'a t = 'a M.t)
    (P : Sample.PACK) : Requirement.OUTPUT
