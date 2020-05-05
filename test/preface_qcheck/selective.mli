(** Generate a suite for [Preface_specs.SELECTIVE]. *)

module Make
    (S : Preface_specs.SELECTIVE
           with type ('a, 'b) either = ('a, 'b) Preface_core.Either.t)
    (R : Requirement.INPUT_T1 with type 'a t = 'a S.t)
    (P : Sample.PACK) : Requirement.OUTPUT

(** Make a suite for Rigid Selective (Selective which are not monads. *)

module Make_rigid
    (S : Preface_specs.SELECTIVE
           with type ('a, 'b) either = ('a, 'b) Preface_core.Either.t)
    (R : Requirement.INPUT_T1 with type 'a t = 'a S.t)
    (P : Sample.PACK) : Requirement.OUTPUT
