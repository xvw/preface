(* Implementation of Validation's test from: 

   https://github.com/snowleopard/selective/blob/master/examples/Validation.hs 
*)

module Shape = struct
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
    let open Preface.Validate.Selective in
    if_ choice (circle <$> r) (rectangle <$> w <*> h)
  ;;

  let testable =
    Alcotest.testable (Preface.Validate.pp pp) (Preface.Validate.equal eq)
  ;;
end

module Nel = Preface.Nonempty_list

let validation_shape_1 () =
  let open Preface.Validate in
  let expected = valid (Shape.circle 1)
  and computed =
    Shape.make (valid true) (valid 1)
      (invalid (Nel.create (Shape.Fail "width")))
      (invalid (Nel.create (Shape.Fail "width")))
  in
  Alcotest.check Shape.testable "Should be a valid circle" expected computed
;;

let validation_shape_2 () =
  let open Preface.Validate in
  let expected = valid (Shape.rectangle 2 3)
  and computed =
    Shape.make (valid false)
      (invalid (Nel.create (Shape.Fail "radius")))
      (valid 2) (valid 3)
  in
  Alcotest.check Shape.testable "Should be a valid rectangle" expected computed
;;

let validation_shape_3 () =
  let open Preface.Validate in
  let expected = invalid (Nel.create (Shape.Fail "height"))
  and computed =
    Shape.make (valid false)
      (invalid (Nel.create (Shape.Fail "radius")))
      (valid 2)
      (invalid (Nel.create (Shape.Fail "height")))
  in
  Alcotest.check Shape.testable "Should be a invalid with height" expected
    computed
;;

let validation_shape_4 () =
  let open Preface.Validate in
  let expected =
    invalid Nel.(Shape.Fail "width" :: create (Shape.Fail "height"))
  and computed =
    Shape.make (valid false)
      (invalid (Nel.create (Shape.Fail "radius")))
      (invalid (Nel.create (Shape.Fail "width")))
      (invalid (Nel.create (Shape.Fail "height")))
  in

  Alcotest.check Shape.testable "Should be invalid with width and height"
    expected computed
;;

let cases =
  [
    ( "Shape validation using Validate"
    , let open Alcotest in
      [
        test_case "Simple validation of Circle" `Quick validation_shape_1
      ; test_case "Simple validation of Rectangle" `Quick validation_shape_2
      ; test_case "Simple validation of Rectangle with one failure" `Quick
          validation_shape_3
      ; test_case "Simple validation of Rectangle with two failures" `Quick
          validation_shape_4
      ] )
  ]
;;
