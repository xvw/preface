let subject a =
  Alcotest.testable
    (Preface.Validate.pp (Alcotest.pp a))
    (Preface.Validate.equal (Alcotest.equal a))
;;

module Formlet = struct
  module Nel = Preface.Nonempty_list

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
    let open Preface.Validate in
    if age > 7 then valid age else invalid (Nel.create (Invalid_age age))
  ;;

  let validate_name which name =
    let open Preface.Validate in
    if String.length name > 1
    then valid name
    else invalid (Nel.create (Invalid_name (which, name)))
  ;;

  let validate_rules checked =
    let open Preface.Validate in
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

let validation_formlet_valid () =
  let open Preface.Validate in
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
  let open Preface.Validate in
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
  let open Preface.Validate in
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
  let open Preface.Validate in
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
  let open Preface.Validate in
  let expected =
    invalid
      (let open Formlet in
      let open Nel in
      Invalid_age (-5)
      ::
      Invalid_name ("firstname", "")
      :: Invalid_name ("lastname", "-") :: create Unchecked_rules)
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

let cases =
  [
    ( "Formlet using Validate"
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
      ] )
  ]
;;
