module Functor_test = Support.Functor (struct
  include Preface_stdlib.Validation
  include Functor
end)

module Applicative_test = Support.Applicative (struct
  include Preface_stdlib.Validation
  include Applicative
end)

module Monad_test = Support.Monad (struct
  include Preface_stdlib.Validation
  include Monad
end)

open Preface_stdlib.Validation

let subject a = Alcotest.testable (pp (Alcotest.pp a)) (eq ( = ))

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

  let validate_age age = if age > 7 then Ok age else Error [ Invalid_age age ]

  let validate_name which name =
    if String.length name > 1
    then Ok name
    else Error [ Invalid_name (which, name) ]
  ;;

  let validate_rules checked =
    if checked then Ok `Yes else Error [ Unchecked_rules ]
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

let validation_formlet_valid () =
  let expected =
    Ok
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
  let expected = Error Formlet.[ Invalid_age (-5) ]
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
    Error
      Formlet.[ Invalid_name ("firstname", ""); Invalid_name ("lastname", "-") ]
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
  let expected = Error Formlet.[ Unchecked_rules ]
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
    Error
      Formlet.
        [
          Invalid_age (-5)
        ; Invalid_name ("firstname", "")
        ; Invalid_name ("lastname", "-")
        ; Unchecked_rules
        ]
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
end

let testable = Alcotest.testable (pp Shape.pp) (eq Shape.eq)

let validation_shape_1 () =
  let expected = Ok (Shape.circle 1)
  and computed =
    Shape.make (Ok true) (Ok 1)
      (Error [ Shape.Fail "width" ])
      (Error [ Shape.Fail "height" ])
  in
  Alcotest.check testable "Should be a valid circle" expected computed
;;

let validation_shape_2 () =
  let expected = Ok (Shape.rectangle 2 3)
  and computed =
    Shape.make (Ok false) (Error [ Shape.Fail "radius" ]) (Ok 2) (Ok 3)
  in
  Alcotest.check testable "Should be a valid rectangle" expected computed
;;

let validation_shape_3 () =
  let expected = Error [ Shape.Fail "height" ]
  and computed =
    Shape.make (Ok false)
      (Error [ Shape.Fail "radius" ])
      (Ok 2)
      (Error [ Shape.Fail "height" ])
  in
  Alcotest.check testable "Should be a invalid with height" expected computed
;;

let validation_shape_4 () =
  let expected = Error [ Shape.Fail "width"; Shape.Fail "height" ]
  and computed =
    Shape.make (Ok false)
      (Error [ Shape.Fail "radius" ])
      (Error [ Shape.Fail "width" ])
      (Error [ Shape.Fail "height" ])
  in
  Alcotest.check testable "Should be invalid with width and height" expected
    computed
;;

let test_cases =
  [
    ("Validation Functor", Functor_test.cases)
  ; ("Validation Applicative", Applicative_test.cases)
  ; ("Validation Monad", Monad_test.cases)
  ; ( "Validation use cases"
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
      ] )
  ]
;;
