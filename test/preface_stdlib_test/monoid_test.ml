module N = Preface_core.Nonempty_list

module S = Preface_make.Monoid.Via_concat_and_zero (struct
  type t = string

  let concat = ( ^ )

  let zero = ""
end)

module I = Preface_make.Monoid.Via_concat_and_zero (struct
  type t = int

  let concat = ( + )

  let zero = 0
end)

let string_concat () =
  let expected = "FooBar"
  and computed = S.("Foo" ++ "Bar") in
  Alcotest.(check string) "string_concat" expected computed
;;

let int_concat () =
  let expected = 16
  and computed = I.(6 ++ 10) in
  Alcotest.(check int) "int_concat" expected computed
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

let string_reduce_1 () =
  let expected = "FooBarOCamlPreface"
  and computed = S.reduce [ "Foo"; "Bar"; "OCaml"; "Preface" ] in
  Alcotest.(check string) "string_reduce_1" expected computed
;;

let string_reduce_2 () =
  let expected = ""
  and computed = S.reduce [] in
  Alcotest.(check string) "string_reduce_2" expected computed
;;

let int_reduce_1 () =
  let expected = 10
  and computed = I.reduce [ 1; 2; 3; 4 ] in
  Alcotest.(check int) "int_reduce_1" expected computed
;;

let int_reduce_2 () =
  let expected = 0
  and computed = I.reduce [] in
  Alcotest.(check int) "int_reduce_2" expected computed
;;

let test_cases =
  let open Alcotest in
  [
    ( "Monoid"
    , [
        test_case "String concat" `Quick string_concat
      ; test_case "Int concat" `Quick int_concat
      ; test_case "String times 1" `Quick string_times_1
      ; test_case "String times 2" `Quick string_times_2
      ; test_case "Int times 1" `Quick int_times_1
      ; test_case "Int times 2" `Quick int_times_2
      ; test_case "String reduce_nel 1" `Quick string_reduce_nel_1
      ; test_case "String reduce_nel 2" `Quick string_reduce_nel_2
      ; test_case "Int reduce_nel 1" `Quick int_reduce_nel_1
      ; test_case "Int reduce_nel 2" `Quick int_reduce_nel_2
      ; test_case "String reduce 1" `Quick string_reduce_1
      ; test_case "String reduce 2" `Quick string_reduce_2
      ; test_case "Int reduce 1" `Quick int_reduce_1
      ; test_case "Int reduce 2" `Quick int_reduce_2
      ] )
  ]
;;
