(** Create test suite for [Bifunctor] using [QCheck].

    A [Bifunctor] must satisfy these laws:

    - [bimap id id = id]
    - [fst id = id]
    - [snd id = id]
    - [bimap f g = (fst f) % (snd g)]
    - [bimap (f % g) (h % i) = (bimap f h) % (bimap g i)]
    - [fst (f % g) = (fst f) % (fst g)]
    - [snd (f % g) = (snd f) % (snd g)]

    Like [Functor_laws], the library provides a module [Make] which takes eight
    modules. Also like [Functor_laws], the two first modules are for
    configuration purpose ([Bifunctor] and [Req]) and the four next modules are
    represent the variables and the function involved in the laws.

    - [Bifunctor] the target bifunctor
    - [Req] which handle the [arbitrary] to generate values in the bifunctor
    - [L] a generator for left values in the laws
    - [R] a generator for right values in the laws
    - [F] a generator for function [f] in the laws
    - [G] a generator for function [g] in the laws
    - [H] a generator for function [h] in the laws
    - [I] a generator for function [i] in the laws

    QCheck will generate values and function using these modules (and connecting
    them together).

    Let's try to implement test suite for [Either.Bifunctor]:

    {[
      module Either_bifunctor =
        Bifunctor_laws.Make
          (Bifunctor)
          (struct
            let suite_name = "Either"

            type ('a, 'b) t = ('a, 'b) Bifunctor.t

            let arbitrary = Preface_test.Qcheck_helpers.Arbitrary.either
          end)
          (Preface_test.Qcheck_helpers.Sample.String)
          (Preface_test.Qcheck_helpers.Sample.Int)
          (Preface_test.Qcheck_helpers.Sample.String)
          (Preface_test.Qcheck_helpers.Sample.Int)
          (Preface_test.Qcheck_helpers.Sample.String)
          (Preface_test.Qcheck_helpers.Sample.Int)
    ]}

    Now [Either_bifunctor] provides [cases] which can be executed as an Alcotest
    cases.

    {1 Build a test suite for bifunctor} *)

(** Generate test cases for [Bifunctor]. *)
module Make
    (Bifunctor : Preface_specs.BIFUNCTOR)
    (Req : sig
      type ('a, 'b) t

      val suite_name : string

      val arbitrary :
           'a QCheck.arbitrary
        -> 'b QCheck.arbitrary
        -> ('a, 'b) t QCheck.arbitrary
    end
    with type ('a, 'b) t = ('a, 'b) Bifunctor.t)
    (L : Qcheck_helpers.GENERATOR)
    (R : Qcheck_helpers.GENERATOR)
    (F : Qcheck_helpers.GENERATOR)
    (G : Qcheck_helpers.GENERATOR)
    (H : Qcheck_helpers.GENERATOR)
    (I : Qcheck_helpers.GENERATOR) : Qcheck_helpers.ALCOTEST_SUITE
