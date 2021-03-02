type template_item =
  | Const of string
  | Var of string

type template = template_item list

module Bindings = Map.Make (String)

module Reader = Preface.Make.Reader.Over (struct
  type t = string Bindings.t
end)

let transform_item =
  let open Reader in
  let open Reader.Monad in
  let open Bindings in
  function
  | Const s -> return s
  | Var s ->
    let* env = ask in
    return (if mem s env then find s env else "N/A")
;;

let rec transform =
  let open Reader.Monad in
  function
  | [] -> return ""
  | a :: l ->
    let* ta = transform_item a in
    let* tl = transform l in
    return (ta ^ tl)
;;

let should_transform_constant () =
  let expected = "Hello"
  and computed = Reader.run (transform [ Const "Hello" ]) Bindings.empty in
  Alcotest.(check string) "transform_constant" expected computed
;;

let should_transform_variable () =
  let expected = "Alice"
  and computed =
    Reader.run (transform [ Var "name" ]) (Bindings.singleton "name" "Alice")
  in
  Alcotest.(check string) "transform_variable" expected computed
;;

let should_not_transform_variable () =
  let expected = "N/A"
  and computed = Reader.run (transform [ Var "name" ]) Bindings.empty in
  Alcotest.(check string) "not_transform_variable" expected computed
;;

let should_transform_constant_and_variable () =
  let expected = "Hello, Alice"
  and computed =
    Reader.run
      (transform [ Const "Hello"; Const ", "; Var "name" ])
      (Bindings.singleton "name" "Alice")
  in
  Alcotest.(check string) "transform_constant_and_variable" expected computed
;;

let cases =
  let open Alcotest in
  [
    ( "Templating using Reader"
    , [
        test_case "Should transform a constant" `Quick should_transform_constant
      ; test_case "Should transform a variable" `Quick should_transform_variable
      ; test_case "Should not transform a variable" `Quick
          should_not_transform_variable
      ; test_case "Should not transform a sequence of constants and variable"
          `Quick should_transform_constant_and_variable
      ] )
  ]
;;
