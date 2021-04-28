module I = Preface.Make.Monoid.Via_combine_and_neutral (struct
  type t = Int.t

  let neutral = 0

  let combine x y = x + y
end)

module L = Preface.Traced.Over (Preface.List.Monoid (Int))

let sum_neutral () =
  let open Alcotest in
  let expected = 0
  and computed = L.(extract @@ traced I.reduce) in
  check int "Should be equal" expected computed
;;

let sum_with_element () =
  let open Alcotest in
  let expected = 15
  and computed = L.(trace [ 1; 2; 3; 4; 5 ] @@ traced I.reduce) in
  check int "Should be equal" expected computed
;;

let sum_with_extend () =
  let open Alcotest in
  let sum = L.(traced I.reduce =>> trace [ 1; 2; 3; 4; 5 ]) in
  let expected = 25
  and computed = L.trace [ 10 ] sum in
  check int "Should be equal" expected computed
;;

let cases =
  let open Alcotest in
  [
    ( "Traced"
    , [
        test_case "Test for traced with neutral sum" `Quick sum_neutral
      ; test_case "Test for traced sum" `Quick sum_with_element
      ; test_case "Test for traced sum with extend" `Quick sum_with_extend
      ] )
  ]
;;
