(** Create test suite for [Applicative] using [QCheck].

    An [Applicative] must satisfy these laws:

    - [pure id <*> x = x] Identity
    - [pure f <*> pure x = pure (f x)] Homomorphism
    - [u <*> pure x = pure ((|>) x) <*> u] Interchange
    - [pure ( % ) <*> u <*> v <*> w = u <*> (v <*> w)] Composition

    Like in [Functor_laws], the library provides the module [Make] which takes
    seven modules. Also like [Functor_laws], the two first modules are for
    configuration purpose ([Applicative] and [Req]) and the five next modules
    represent the variables and the functions related to the laws.

    - [Applicative] the target applicative
    - [Req] which handle the [arbitrary] to generate values in the applicative
    - [X] a generator for values [x] in the laws
    - [F] a generator for function [f] in the laws
    - [U] a generator for function [u] in the laws
    - [V] a generator for function [v] in the laws
    - [W] a generator for function [w] in the laws

    QCheck will generate values and function using these modules (and connecting
    them together).

    Let's try to implement test suite for [Validation.Applicative]:

    {[
      module Validation_applicative =
        Preface_test.Applicative_laws
          (* The Validation applicative *)
          (Preface_stdlib.Validation.Applicative)
          (* An arbitrary for Validation, given by Qcheck_helpers *)
          (struct
            type 'a t = 'a Preface_stdlib.Validation.Applicative.t

            let suite_name = "Validation"

            let arbitrary = Preface_test.Qcheck_helpers.Arbitrary.validation
          end)
          (* Now we give types for x, f, u, v and w*)
          (Preface_test.Qcheck_helpers.Sample.Int)
          (Preface_test.Qcheck_helpers.Sample.String)
          (Preface_test.Qcheck_helpers.Sample.Int)
          (Preface_test.Qcheck_helpers.Sample.String)
          (Preface_test.Qcheck_helpers.Sample.Int)
    ]}

    Now [Validation_applicative] provides [cases] which can be executed as an
    Alcotest cases.

    {1 Build a test suite for Applicative} *)

(** Generate test cases for [Applicative]. *)
module Make
    (Applicative : Preface_specs.APPLICATIVE)
    (Req : Qcheck_helpers.REQ with type 'a t = 'a Applicative.t)
    (X : Qcheck_helpers.GENERATOR)
    (F : Qcheck_helpers.GENERATOR)
    (U : Qcheck_helpers.GENERATOR)
    (V : Qcheck_helpers.GENERATOR)
    (W : Qcheck_helpers.GENERATOR) : Qcheck_helpers.ALCOTEST_SUITE

(** Generate test cases for [Applicative] using a post hook. *)
module Make_with_post_hook
    (Applicative : Preface_specs.APPLICATIVE)
    (Req : Qcheck_helpers.REQ with type 'a t = 'a Applicative.t)
    (Hook : Qcheck_helpers.HOOK with type 'a t = 'a Applicative.t)
    (X : Qcheck_helpers.GENERATOR)
    (F : Qcheck_helpers.GENERATOR)
    (U : Qcheck_helpers.GENERATOR)
    (V : Qcheck_helpers.GENERATOR)
    (W : Qcheck_helpers.GENERATOR) : Qcheck_helpers.ALCOTEST_SUITE
