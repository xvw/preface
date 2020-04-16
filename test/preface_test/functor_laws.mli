(** Create test suite for [Functor] using [QCheck].

    A [Functor] must satisfy these laws:

    - [map id x = x] Identity
    - [map (f % g) x = (map f % map g) x] Morphism preservation

    In order ton define a set of test for each law, the library provides the
    module [Make] which takes five modules. Two modules for the configuration,
    [Functor] and [Req], and three modules representing variables related to the
    laws, [X], [F] and [G]. [F] and [G] define their return type.

    - [Functor] which is the target functor
    - [Req] which handle the [arbitrary] to generate values in the functor
    - [X] a generator for values [x] in the laws
    - [F] a generator for function [f] in the laws
    - [G] a generator for function [g] in the laws

    QCheck will generate values and function using these modules (and connecting
    them together). For example, with a module [G] which handle an [string] and
    a module [X] which handle an [int], the function [g] (from the laws) will
    have the type [(int -> string)] and if the module [F] handle the type
    [float], the function [f] in the laws will have the type
    [(string -> float)].

    Let's try to implement test suite for [List.Functor] with function [f] and
    [g] respectively with the type [(string -> int)] and [(int -> string)]. So
    the type of [x] mut be [int]:

    {[
      module List_functor =
        Preface_test.Functor_laws
          (* The list functor. *)
          (Preface_stdlib.List.Functor)
          (* An arbitrary for a list Functor, given by QCheck. *)
          (struct
            type 'a t = 'a Preface_stdlib.List.Functor.t

            let suite_name = "Validation"

            let arbitrary = QCheck.small_list
          end)
          (* Now the type for [x], the output of [f] and the output of [g]. *)
          (Preface_test.Qcheck_helpers.Sample.Int)
          (Preface_test.Qcheck_helpers.Sample.Int)
          (Preface_test.Qcheck_helpers.Sample.String)
    ]}

    Now [List_functor] provides [cases] which can be executed as an Alcotest
    cases.

    {1 Build a test suite for Functor} *)

(** Generate test cases for [Functor]. *)
module Make
    (Functor : Preface_specs.FUNCTOR)
    (Req : Qcheck_helpers.REQ with type 'a t = 'a Functor.t)
    (X : Qcheck_helpers.GENERATOR)
    (F : Qcheck_helpers.GENERATOR)
    (G : Qcheck_helpers.GENERATOR) : Qcheck_helpers.ALCOTEST_SUITE

(** Generate test cases for [Functor] using a post hook. *)
module Make_with_post_hook
    (Functor : Preface_specs.FUNCTOR)
    (Req : Qcheck_helpers.REQ with type 'a t = 'a Functor.t)
    (Hook : Qcheck_helpers.HOOK with type 'a t = 'a Functor.t)
    (X : Qcheck_helpers.GENERATOR)
    (F : Qcheck_helpers.GENERATOR)
    (G : Qcheck_helpers.GENERATOR) : Qcheck_helpers.ALCOTEST_SUITE
