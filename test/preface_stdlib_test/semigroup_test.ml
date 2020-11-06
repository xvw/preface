module N = Preface_core.Nonempty_list

module S = Preface_make.Semigroup.Via_combine (struct
  type t = string

  let combine = ( ^ )
end)

module I = Preface_make.Semigroup.Via_combine (struct
  type t = int

  let combine = ( + )
end)

let string_combine () =
  let expected = "FooBar"
  and computed = S.("Foo" <|> "Bar") in
  Alcotest.(check string) "string_combine" expected computed
;;

let int_combine () =
  let expected = 16
  and computed = I.(6 <|> 10) in
  Alcotest.(check int) "int_combine" expected computed
;;

let string_times_1 () =
  let expected = Some "FooFooFooFoo"
  and computed = S.times 4 "Foo" in
  Alcotest.(check (option string)) "string_times_1" expected computed
;;

let string_times_2 () =
  let expected = None
  and computed = S.times 0 "Foo" in
  Alcotest.(check (option string)) "string_times_2" expected computed
;;

let int_times_1 () =
  let expected = Some 16
  and computed = I.times 4 4 in
  Alcotest.(check (option int)) "int_times_1" expected computed
;;

let int_times_2 () =
  let expected = None
  and computed = I.times 0 4 in
  Alcotest.(check (option int)) "int_times_2" expected computed
;;

let string_reduce_nel_1 () =
  let expected = "FooBarOCamlPreface"
  and computed = S.reduce_nel N.("Foo" :: "Bar" :: "OCaml" :: Last "Preface") in
  Alcotest.(check string) "string_reduce_nel_1" expected computed
;;

let string_reduce_nel_2 () =
  let expected = "Foo"
  and computed = S.reduce_nel N.(Last "Foo") in
  Alcotest.(check string) "string_reduce_nel_2" expected computed
;;

let int_reduce_nel_1 () =
  let expected = 10
  and computed = I.reduce_nel N.(0 :: 1 :: 2 :: 3 :: Last 4) in
  Alcotest.(check int) "int_reduce_nel_1" expected computed
;;

let int_reduce_nel_2 () =
  let expected = 10
  and computed = I.reduce_nel N.(Last 10) in
  Alcotest.(check int) "int_reduce_nel_2" expected computed
;;

let test_cases =
  let open Alcotest in
  [
    ( "Semigroup"
    , [
        test_case "String combine" `Quick string_combine
      ; test_case "Int combine" `Quick int_combine
      ; test_case "String times 1" `Quick string_times_1
      ; test_case "String times 2" `Quick string_times_2
      ; test_case "Int times 1" `Quick int_times_1
      ; test_case "Int times 2" `Quick int_times_2
      ; test_case "String reduce_nel 1" `Quick string_reduce_nel_1
      ; test_case "String reduce_nel 2" `Quick string_reduce_nel_2
      ; test_case "Int reduce_nel 1" `Quick int_reduce_nel_1
      ; test_case "Int reduce_nel 2" `Quick int_reduce_nel_2
      ] )
  ]
;;
