(** Create test suite for [Selective] using [QCheck]. *)

(** {1 Build a test suite for selective} *)

(** A rigid selective is a selective where [Selective.apply] is equivalent to
    [Applicative.apply]. *)

module Make_for_rigid
    (Selective : Preface_specs.SELECTIVE
                   with type ('a, 'b) either = ('a, 'b) Preface_core.Either.t)
    (Req : Qcheck_helpers.REQ with type 'a t = 'a Selective.t)
    (X : Qcheck_helpers.GENERATOR)
    (Y : Qcheck_helpers.GENERATOR)
    (Z : Qcheck_helpers.GENERATOR) : Qcheck_helpers.ALCOTEST_SUITE

(** A rigid selective is a selective where [Selective.apply] is not equivalent
    to [Applicative.apply]. *)

module Make_for_non_rigid
    (Selective : Preface_specs.SELECTIVE
                   with type ('a, 'b) either = ('a, 'b) Preface_core.Either.t)
    (Req : Qcheck_helpers.REQ with type 'a t = 'a Selective.t)
    (X : Qcheck_helpers.GENERATOR)
    (Y : Qcheck_helpers.GENERATOR)
    (Z : Qcheck_helpers.GENERATOR) : Qcheck_helpers.ALCOTEST_SUITE
