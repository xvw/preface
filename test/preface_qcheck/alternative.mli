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

(** Test suite for Alternatives which satisfy the right absorption law. *)
module Make_right_absorption
    (A : Preface_specs.ALTERNATIVE)
    (R : Requirement.INPUT_T1 with type 'a t = 'a A.t)
    (P : Sample.PACK) : Requirement.OUTPUT

(** Test suite for Alternatives which satisfy the right distributivity of apply
    law. *)
module Make_right_distributivity_of_apply
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

(** Test suite for Alternatives which satisfy the right absorption law and with
    post hook application. *)
module Make_hooked_right_absorption
    (A : Preface_specs.ALTERNATIVE)
    (R : Requirement.INPUT_T1 with type 'a t = 'a A.t)
    (Hook : Requirement.HOOK with type 'a t = 'a A.t)
    (P : Sample.PACK) : Requirement.OUTPUT

(** Test suite for Alternatives which satisfy the right distributivity of apply
    law and with post hook application. *)
module Make_hooked_right_distributivity_of_apply
    (A : Preface_specs.ALTERNATIVE)
    (R : Requirement.INPUT_T1 with type 'a t = 'a A.t)
    (Hook : Requirement.HOOK with type 'a t = 'a A.t)
    (P : Sample.PACK) : Requirement.OUTPUT
