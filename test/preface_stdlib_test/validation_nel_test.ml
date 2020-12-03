module Nel = Preface_stdlib.Nonempty_list
module Option = Preface_stdlib.Option
module Validation = Preface_stdlib.Validate

let subject a =
  Alcotest.testable (Validation.pp (Alcotest.pp a)) (Validation.equal ( = ))
;;

module Formlet = struct
  type t = {
      age : int
    ; firstname : string
    ; lastname : string
    ; checked_rules : [ `Yes ]
  }

  let make age firstname lastname checked_rules =
    { age; firstname; lastname; checked_rules }
  ;;

  exception Invalid_age of int

  exception Invalid_name of (string * string)

  exception Unchecked_rules

  let validate_age age =
    let open Validation in
    if age > 7 then valid age else invalid (Nel.create (Invalid_age age))
  ;;

  let validate_name which name =
    let open Validation in
    if String.length name > 1
    then valid name
    else invalid (Nel.create (Invalid_name (which, name)))
  ;;

  let validate_rules checked =
    let open Validation in
    if checked then valid `Yes else invalid (Nel.create Unchecked_rules)
  ;;

  let eq a b =
    a.age = b.age && a.firstname = b.firstname && a.lastname = a.lastname
  ;;

  let pp ppf form =
    Format.fprintf ppf "Formlet(%d, %s, %s)" form.age form.firstname
      form.lastname
  ;;

  let testable = Alcotest.testable pp eq
end

open Validation

let validation_formlet_valid () =
  let expected =
    valid
      Formlet.
        {
          age = 30
        ; firstname = "Xavier"
        ; lastname = "Van de Woestyne"
        ; checked_rules = `Yes
        }
  and computed =
    let open Applicative.Infix in
    let open Formlet in
    make
    <$> validate_age 30
    <*> validate_name "firstname" "Xavier"
    <*> validate_name "lastname" "Van de Woestyne"
    <*> validate_rules true
  in
  Alcotest.(check (subject Formlet.testable))
    "Should be valid" expected computed
;;

let validation_formlet_invalid1 () =
  let expected = invalid Formlet.(Nel.create (Invalid_age (-5)))
  and computed =
    let open Applicative.Infix in
    let open Formlet in
    make
    <$> validate_age (-5)
    <*> validate_name "firstname" "Xavier"
    <*> validate_name "lastname" "Van de Woestyne"
    <*> validate_rules true
  in
  Alcotest.(check (subject Formlet.testable))
    "Should be invalid" expected computed
;;

let validation_formlet_invalid2 () =
  let expected =
    invalid
      (let open Formlet in
      let open Nel in
      Invalid_name ("firstname", "") :: create (Invalid_name ("lastname", "-")))
  and computed =
    let open Applicative.Infix in
    let open Formlet in
    make
    <$> validate_age 30
    <*> validate_name "firstname" ""
    <*> validate_name "lastname" "-"
    <*> validate_rules true
  in
  Alcotest.(check (subject Formlet.testable))
    "Should be invalid" expected computed
;;

let validation_formlet_invalid3 () =
  let expected = invalid Formlet.(Nel.create Unchecked_rules)
  and computed =
    let open Applicative.Infix in
    let open Formlet in
    make
    <$> validate_age 30
    <*> validate_name "firstname" "Xavier"
    <*> validate_name "lastname" "Van de Woestyne"
    <*> validate_rules false
  in
  Alcotest.(check (subject Formlet.testable))
    "Should be invalid" expected computed
;;

let validation_formlet_invalid4 () =
  let expected =
    invalid
      (let open Formlet in
      let open Nel in
      Invalid_age (-5)
      :: Invalid_name ("firstname", "")
      :: Invalid_name ("lastname", "-")
      :: create Unchecked_rules)
  and computed =
    let open Applicative.Infix in
    let open Formlet in
    make
    <$> validate_age (-5)
    <*> validate_name "firstname" ""
    <*> validate_name "lastname" "-"
    <*> validate_rules false
  in
  Alcotest.(check (subject Formlet.testable))
    "Should be invalid" expected computed
;;

module Shape = struct
  (* Implementation of Validation's test from:
     https://github.com/snowleopard/selective/blob/master/examples/Validation.hs
  *)

  type radius = int

  type width = int

  type height = int

  type shape =
    | Circle of radius
    | Rectangle of (width * height)

  let eq l r =
    match (l, r) with
    | (Circle x, Circle y) -> x = y
    | (Rectangle (w, h), Rectangle (w2, h2)) -> w = w2 && h = h2
    | _ -> false
  ;;

  let pp ppf = function
    | Circle x -> Format.fprintf ppf "Circle(%d)" x
    | Rectangle (w, h) -> Format.fprintf ppf "Rectangle(%d, %d)" w h
  ;;

  exception Fail of string

  let circle r = Circle r

  let rectangle w h = Rectangle (w, h)

  let make choice r w h =
    Selective.(if_ choice (circle <$> r) (rectangle <$> w <*> h))
  ;;

  let testable = Alcotest.testable (Validation.pp pp) (Validation.equal eq)
end

let fail s = Shape.Fail s

let validation_shape_1 () =
  let expected = valid (Shape.circle 1)
  and computed =
    Shape.make (valid true) (valid 1)
      (invalid (Nel.create (fail "width")))
      (invalid (Nel.create (fail "width")))
  in
  Alcotest.check Shape.testable "Should be a valid circle" expected computed
;;

let validation_shape_2 () =
  let expected = valid (Shape.rectangle 2 3)
  and computed =
    Shape.make (valid false)
      (invalid (Nel.create (fail "radius")))
      (valid 2) (valid 3)
  in
  Alcotest.check Shape.testable "Should be a valid rectangle" expected computed
;;

let validation_shape_3 () =
  let expected = invalid (Nel.create (fail "height"))
  and computed =
    Shape.make (valid false)
      (invalid (Nel.create (fail "radius")))
      (valid 2)
      (invalid (Nel.create (fail "height")))
  in
  Alcotest.check Shape.testable "Should be a invalid with height" expected
    computed
;;

let validation_shape_4 () =
  let expected = invalid Nel.(fail "width" :: create (fail "height"))
  and computed =
    Shape.make (valid false)
      (invalid (Nel.create (fail "radius")))
      (invalid (Nel.create (fail "width")))
      (invalid (Nel.create (fail "height")))
  in

  Alcotest.check Shape.testable "Should be invalid with width and height"
    expected computed
;;

open Nel.Applicative.Traversable (Option.Applicative)

let nel_subject a = Alcotest.(testable (Nel.pp (pp a)) (Nel.equal ( = )))

let should_traverse_without_failure_app () =
  let expected = Some Nel.(1 :: 2 :: create 3)
  and computed = sequence Nel.(Some 1 :: Some 2 :: create (Some 3)) in
  Alcotest.(check (option (nel_subject int)))
    "should_traverse_without_failure" expected computed
;;

let should_traverse_with_failure_app () =
  let expected = None
  and computed = sequence Nel.(Some 1 :: None :: create (Some 3)) in
  Alcotest.(check (option (nel_subject int)))
    "should_traverse_with_failure" expected computed
;;

open Nel.Monad.Traversable (Option.Monad)

let should_traverse_without_failure_monad () =
  let expected = Some Nel.(1 :: 2 :: create 3)
  and computed = sequence Nel.(Some 1 :: Some 2 :: create (Some 3)) in
  Alcotest.(check (option (nel_subject int)))
    "should_traverse_without_failure" expected computed
;;

let should_traverse_with_failure_monad () =
  let expected = None
  and computed = sequence Nel.(Some 1 :: None :: create (Some 3)) in
  Alcotest.(check (option (nel_subject int)))
    "should_traverse_with_failure" expected computed
;;

let test_cases =
  [
    ( "Validation (with nonempty list) use cases"
    , let open Alcotest in
      [
        test_case "Simple validation with success" `Quick
          validation_formlet_valid
      ; test_case "Simple validation with failure (for age)" `Quick
          validation_formlet_invalid1
      ; test_case "Simple validation with failure (for firstname and lastname)"
          `Quick validation_formlet_invalid2
      ; test_case "Simple validation with failure (unchecked rules)" `Quick
          validation_formlet_invalid3
      ; test_case "Simple validation with failure (everything is bad)" `Quick
          validation_formlet_invalid4
      ; test_case "Simple validation of Circle" `Quick validation_shape_1
      ; test_case "Simple validation of Rectangle" `Quick validation_shape_2
      ; test_case "Simple validation of Rectangle with one failure" `Quick
          validation_shape_3
      ; test_case "Simple validation of Rectangle with two failures" `Quick
          validation_shape_4
      ; test_case
          "Traverse through nonempty list over option with success using \
           Applicative"
          `Quick should_traverse_without_failure_app
      ; test_case
          "Traverse through nonempty list over option with failure using \
           Applicative"
          `Quick should_traverse_with_failure_app
      ; test_case
          "Traverse through nonempty list over option with success using Monad"
          `Quick should_traverse_without_failure_monad
      ; test_case
          "Traverse through nonempty list over option with failure using Monad"
          `Quick should_traverse_with_failure_monad
      ] )
  ]
;;
