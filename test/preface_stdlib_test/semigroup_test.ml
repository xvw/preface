module S = Preface_make.Semigroup.Via_concat (struct
  type t = string

  let concat = ( ^ )
end)

module I = Preface_make.Semigroup.Via_concat (struct
  type t = int

  let concat = ( + )
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

let string_reduce_1 () =
  let expected = "FooBarOCamlPreface"
  and computed = S.reduce ("Foo", [ "Bar"; "OCaml"; "Preface" ]) in
  Alcotest.(check string) "string_reduce_1" expected computed
;;

let string_reduce_2 () =
  let expected = "Foo"
  and computed = S.reduce ("Foo", []) in
  Alcotest.(check string) "string_reduce_2" expected computed
;;

let int_reduce_1 () =
  let expected = 10
  and computed = I.reduce (0, [ 1; 2; 3; 4 ]) in
  Alcotest.(check int) "int_reduce_1" expected computed
;;

let int_reduce_2 () =
  let expected = 10
  and computed = I.reduce (10, []) in
  Alcotest.(check int) "int_reduce_2" expected computed
;;

let test_cases =
  let open Alcotest in
  [
    ( "Semigroup"
    , [
        test_case "String concat" `Quick string_concat
      ; test_case "Int concat" `Quick int_concat
      ; test_case "String times 1" `Quick string_times_1
      ; test_case "String times 2" `Quick string_times_2
      ; test_case "Int times 1" `Quick int_times_1
      ; test_case "Int times 2" `Quick int_times_2
      ; test_case "String reduce 1" `Quick string_reduce_1
      ; test_case "String reduce 2" `Quick string_reduce_2
      ; test_case "Int reduce 1" `Quick int_reduce_1
      ; test_case "Int reduce 2" `Quick int_reduce_2
      ] )
  ]
;;
