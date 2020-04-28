module Continuation = Preface_stdlib.Continuation
open Continuation

let should_map () =
  let open Continuation.Functor in
  let expected = 42 in
  let computed = string_of_int <$> (( * ) 2 <$> pure 21) in
  Alcotest.(check int) "should_map" expected
  @@ computed.run (fun x -> int_of_string x)
;;

let should_apply () =
  let open Continuation.Applicative in
  let expected = 42 in
  let computed = pure (( * ) 2) <*> pure 21 in
  Alcotest.(check int) "should_apply" expected @@ computed.run (fun x -> x)
;;

let should_bind () =
  let add x y = pure (x + y)
  and mult x y = pure (x * y)
  and to_string v = pure (string_of_int v) in
  let open Continuation.Monad in
  let expected = "42" in
  let computed = add 3 4 >>= mult 3 >>= mult 2 >>= to_string in
  Alcotest.(check string) "should_bind" expected @@ computed.run (fun x -> x)
;;

let test_cases =
  let open Alcotest in
  [
    ( "Continuation"
    , [
        test_case "Map" `Quick should_map
      ; test_case "Apply" `Quick should_apply
      ; test_case "Bind" `Quick should_bind
      ] )
  ]
;;
