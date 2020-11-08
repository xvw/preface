module Validation = struct
  module V = Preface_stdlib.Validation
  module Alt = Preface_stdlib.List.Alternative

  let valid = V.valid

  let invalid = V.invalid

  type error = exn Alt.t

  type 'a t = ('a, error) V.t

  module ErrorT = struct
    type t = error
  end

  module Misc : sig
    val pure : 'a -> 'a t

    val eq : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val pp :
      (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  end = struct
    let pure = V.valid

    let eq f = V.eq f Preface_stdlib.(List.eq Exn.eq)

    let pp f = V.pp f Preface_stdlib.(List.pp Exn.pp)
  end

  module Functor = struct
    include V.Functor (ErrorT)
    include Misc
  end

  module Applicative = struct
    include V.Applicative (Alt) (Preface_stdlib.Exn)
    include Misc
  end

  module Monad = struct
    include V.Monad (ErrorT)
    include Misc
  end

  module Selective = V.Selective (Alt) (Preface_stdlib.Exn)
end

module Functor_test = Support.Functor (Validation.Functor)
module Applicative_test = Support.Applicative (Validation.Applicative)
module Monad_test = Support.Monad (Validation.Monad)
open Validation

let subject a =
  Validation.Misc.(Alcotest.testable (pp (Alcotest.pp a)) (eq ( = )))
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
    if age > 7 then valid age else invalid [ Invalid_age age ]
  ;;

  let validate_name which name =
    if String.length name > 1
    then valid name
    else invalid [ Invalid_name (which, name) ]
  ;;

  let validate_rules checked =
    if checked then valid `Yes else invalid [ Unchecked_rules ]
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
  let expected = invalid Formlet.[ Invalid_age (-5) ]
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
  let expected = invalid Formlet.[ Unchecked_rules ]
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

let testable = Misc.(Alcotest.testable (pp Shape.pp) (eq Shape.eq))

let validation_shape_1 () =
  let expected = valid (Shape.circle 1)
  and computed =
    Shape.make (valid true) (valid 1)
      (invalid [ Shape.Fail "width" ])
      (invalid [ Shape.Fail "height" ])
  in
  Alcotest.check testable "Should be a valid circle" expected computed
;;

let validation_shape_2 () =
  let expected = valid (Shape.rectangle 2 3)
  and computed =
    Shape.make (valid false)
      (invalid [ Shape.Fail "radius" ])
      (valid 2) (valid 3)
  in
  Alcotest.check testable "Should be a valid rectangle" expected computed
;;

let validation_shape_3 () =
  let expected = invalid [ Shape.Fail "height" ]
  and computed =
    Shape.make (valid false)
      (invalid [ Shape.Fail "radius" ])
      (valid 2)
      (invalid [ Shape.Fail "height" ])
  in
  Alcotest.check testable "Should be a invalid with height" expected computed
;;

let validation_shape_4 () =
  let expected = invalid [ Shape.Fail "width"; Shape.Fail "height" ]
  and computed =
    Shape.make (valid false)
      (invalid [ Shape.Fail "radius" ])
      (invalid [ Shape.Fail "width" ])
      (invalid [ Shape.Fail "height" ])
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
