module Continuation = Preface.Continuation

let should_map () =
  let open Continuation.Functor in
  let expected = 42 in
  let computed = string_of_int <$> (( * ) 2 <$> Continuation.pure 21) in
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
  let open Continuation.Monad in
  let add x y = return (x + y)
  and mult x y = return (x * y)
  and to_string v = return (string_of_int v) in
  let expected = "42" in
  let computed = add 3 4 >>= mult 3 >>= mult 2 >>= to_string in
  Alcotest.(check string) "should_bind" expected @@ computed.run (fun x -> x)
;;

let cases =
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
