open Preface.Stream
open Preface.Stream.Comonad

let nats =
  let rec nats n = stream n (lazy (nats (n + 1))) in
  nats 0

let should_extract () =
  let expected = 0 and computed = extract nats in
  Alcotest.(check int) "should_extract" expected computed

let test_cases =
  let open Alcotest in
  ( "Stream Comonad"
  , [ test_case "Extract" `Quick should_extract
    ; test_case "Extract" `Quick should_extract ] )
