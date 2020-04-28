module Functor_test = Support.Functor (struct
  include Preface_stdlib.Try
  include Functor
end)

module Applicative_test = Support.Applicative (struct
  include Preface_stdlib.Try
  include Applicative
end)

module Monad_test = Support.Monad (struct
  include Preface_stdlib.Try
  include Monad
end)

open Preface_stdlib.Try

let subject a = Alcotest.testable (pp (Alcotest.pp a)) (eq ( = ))

exception Invalid_name of string

exception Invalid_age of int

let validate_name (name, age) =
  if String.length name > 1 then Ok (name, age) else Error (Invalid_name name)
;;

let validate_age (name, age) =
  if age >= 18 then Ok (name, age) else Error (Invalid_age age)
;;

let simple_validation_success () =
  let open Monad.Infix in
  let computed = Monad.return ("Xavier", 30) >>= validate_name >>= validate_age
  and expected = Ok ("Xavier", 30) in
  Alcotest.(check (subject (pair string int)))
    "should be valid" expected computed
;;

let simple_validation_failure_1 () =
  let open Monad.Infix in
  let computed = Monad.return ("X", 30) >>= validate_name >>= validate_age
  and expected = Error (Invalid_name "X") in
  Alcotest.(check (subject (pair string int)))
    "should be invalid" expected computed
;;

let simple_validation_failure_2 () =
  let open Monad.Infix in
  let computed = Monad.return ("Xavier", 17) >>= validate_name >>= validate_age
  and expected = Error (Invalid_age 17) in
  Alcotest.(check (subject (pair string int)))
    "should be invalid" expected computed
;;

let test_cases =
  [
    ("Try Functor", Functor_test.cases)
  ; ("Try Applicative", Applicative_test.cases)
  ; ("Try Monad", Monad_test.cases)
  ; ( "Try use cases"
    , let open Alcotest in
      [
        test_case "Simple validation with success" `Quick
          simple_validation_success
      ; test_case "Simple validation failure (for name)" `Quick
          simple_validation_failure_1
      ; test_case "Simple validation failure (for age)" `Quick
          simple_validation_failure_2
      ] )
  ]
;;
