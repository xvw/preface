(** Create test suite for [Monad] using [QCheck].

    A [Monad] must satisfy these laws:

    - [return x >>= f = f x] Left identity
    - [x >>= return = x] Right identity
    - [(x >>= f) >>= g = x >>= (fun y -> f y >>= g)]

    And some additional laws (using Kleisli composition, or using the laws of
    natural transformation and [map] and [join]. So, like in [Functor_laws], the
    library provides a module [Make] which takes six modules. Also like
    [Functor_laws], the two first modules are for configuration purpose ([Monad]
    and [Req]) and the four next modules are represent the variables and the
    function involved in the laws.

    - [Monad] the target applicative
    - [Req] which handle the [arbitrary] to generate values in the monad
    - [X] a generator for values [x] in the laws
    - [F] a generator for function [f] in the laws
    - [G] a generator for function [g] in the laws
    - [H] a generator for function [h] (for the Kleisli composition)

    QCheck will generate values and function using these modules (and connecting
    them together).

    Let's try to implement test suite for [Try.Monad]:

    {[
      module Try_monad =
        Preface_test.Monad_laws.Make
          (* The Try monad *)
          (Preface_stdlib.Try.Monad)
          (* An arbitrary for Try, given by Qcheck_helpers *)
          (struct
            type 'a t = 'a Preface_stdlib.Try.Monad.t

            let suite_name = "Try"

            let arbitrary = Preface_test.Qcheck_helpers.Arbitrary.try_
          end)
          (* Now we give types for x, f, g and h*)
          (Preface_test.Qcheck_helpers.Sample.Int)
          (Preface_test.Qcheck_helpers.Sample.String)
          (Preface_test.Qcheck_helpers.Sample.Int)
          (Preface_test.Qcheck_helpers.Sample.String)
    ]}

    Now [Try_monad] provides [cases] which can be executed as an Alcotest cases.

    {1 Build a test suite for monad} *)

(** Generate test cases for [Monad]. *)
module Make
    (Monad : Preface_specs.MONAD)
    (Req : Qcheck_helpers.REQ with type 'a t = 'a Monad.t)
    (X : Qcheck_helpers.GENERATOR)
    (F : Qcheck_helpers.GENERATOR)
    (G : Qcheck_helpers.GENERATOR)
    (H : Qcheck_helpers.GENERATOR) : Qcheck_helpers.ALCOTEST_SUITE

(** Generate test cases for [Monad] using a post hook. *)
module Make_with_post_hook
    (Monad : Preface_specs.MONAD)
    (Req : Qcheck_helpers.REQ with type 'a t = 'a Monad.t)
    (Hook : Qcheck_helpers.HOOK with type 'a t = 'a Monad.t)
    (X : Qcheck_helpers.GENERATOR)
    (F : Qcheck_helpers.GENERATOR)
    (G : Qcheck_helpers.GENERATOR)
    (H : Qcheck_helpers.GENERATOR) : Qcheck_helpers.ALCOTEST_SUITE
