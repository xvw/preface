(** Generate a suite for [Preface_specs.MONAD_PLUS]. *)

(** Test suite for Monad_plus behaviour. *)
module Make_behaviour
    (M : Preface_specs.MONAD_PLUS)
    (R : Requirement.INPUT_T1 with type 'a t = 'a M.t)
    (P : Sample.PACK) : Requirement.OUTPUT

(** Test suite for Monad_plus with monoidal behaviour. *)
module Make_monoidal_laws
    (M : Preface_specs.MONAD_PLUS)
    (R : Requirement.INPUT_T1 with type 'a t = 'a M.t)
    (P : Sample.PACK) : Requirement.OUTPUT

(** Test suite for Monad_plus with left zero behaviour. *)
module Make_left_zero_law
    (M : Preface_specs.MONAD_PLUS)
    (R : Requirement.INPUT_T1 with type 'a t = 'a M.t)
    (P : Sample.PACK) : Requirement.OUTPUT

(** Test suite for Monad_plus with left distribution behaviour. *)
module Make_left_distribution_law
    (M : Preface_specs.MONAD_PLUS)
    (R : Requirement.INPUT_T1 with type 'a t = 'a M.t)
    (P : Sample.PACK) : Requirement.OUTPUT

(** Test suite for Monad_plus with left catch behaviour. *)
module Make_left_catch_law
    (M : Preface_specs.MONAD_PLUS)
    (R : Requirement.INPUT_T1 with type 'a t = 'a M.t)
    (P : Sample.PACK) : Requirement.OUTPUT

(** Test suite for Monad_plus behaviour with post hook application. *)
module Make_hooked_behaviour
    (M : Preface_specs.MONAD_PLUS)
    (R : Requirement.INPUT_T1 with type 'a t = 'a M.t)
    (Hook : Requirement.HOOK with type 'a t = 'a M.t)
    (P : Sample.PACK) : Requirement.OUTPUT

(** Test suite for Monad_plus with monoidal behaviour with post hook
    application. *)
module Make_hooked_monoidal_laws
    (M : Preface_specs.MONAD_PLUS)
    (R : Requirement.INPUT_T1 with type 'a t = 'a M.t)
    (Hook : Requirement.HOOK with type 'a t = 'a M.t)
    (P : Sample.PACK) : Requirement.OUTPUT

(** Test suite for Monad_plus with left zero behaviour with post hook
    application. *)
module Make_hooked_left_zero_law
    (M : Preface_specs.MONAD_PLUS)
    (R : Requirement.INPUT_T1 with type 'a t = 'a M.t)
    (Hook : Requirement.HOOK with type 'a t = 'a M.t)
    (P : Sample.PACK) : Requirement.OUTPUT

(** Test suite for Monad_plus with left distribution behaviour with post hook
    application. *)
module Make_hooked_left_distribution_law
    (M : Preface_specs.MONAD_PLUS)
    (R : Requirement.INPUT_T1 with type 'a t = 'a M.t)
    (Hook : Requirement.HOOK with type 'a t = 'a M.t)
    (P : Sample.PACK) : Requirement.OUTPUT

(** Test suite for Monad_plus with left catch behaviour with post hook
    application. *)
module Make_hooked_left_catch_law
    (M : Preface_specs.MONAD_PLUS)
    (R : Requirement.INPUT_T1 with type 'a t = 'a M.t)
    (Hook : Requirement.HOOK with type 'a t = 'a M.t)
    (P : Sample.PACK) : Requirement.OUTPUT
