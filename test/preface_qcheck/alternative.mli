(** Generate a suite for [Preface_specs.ALTERNATIVE]. *)

(** Test suite for Alternative behaviour. *)
module Make_behaviour
    (A : Preface_specs.ALTERNATIVE)
    (R : Requirement.INPUT_T1 with type 'a t = 'a A.t)
    (P : Sample.PACK) : Requirement.OUTPUT

(** Test suite for Alternative behaviour with monoidal behaviour. *)
module Make_for_monoidal_behaviour
    (A : Preface_specs.ALTERNATIVE)
    (R : Requirement.INPUT_T1 with type 'a t = 'a A.t)
    (P : Sample.PACK) : Requirement.OUTPUT

(** Test suite for Alternative behaviour with post hook application. *)
module Make_hooked_behaviour
    (A : Preface_specs.ALTERNATIVE)
    (R : Requirement.INPUT_T1 with type 'a t = 'a A.t)
    (Hook : Requirement.HOOK with type 'a t = 'a A.t)
    (P : Sample.PACK) : Requirement.OUTPUT

(** Test suite for Alternative behaviour with monoidal behaviour and with post
    hook application. *)
module Make_hooked_for_monoidal_behaviour
    (A : Preface_specs.ALTERNATIVE)
    (R : Requirement.INPUT_T1 with type 'a t = 'a A.t)
    (Hook : Requirement.HOOK with type 'a t = 'a A.t)
    (P : Sample.PACK) : Requirement.OUTPUT
