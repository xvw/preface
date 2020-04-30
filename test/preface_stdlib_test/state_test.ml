open Preface_stdlib.State.Over (struct
  type t = int
end)

let should_get_value () =
  let value = 42 in
  let expected = (value, value)
  and computed = get value in
  Alcotest.(check (pair int int)) "Should retrieve value" expected computed
;;

let should_get_and_set_a_new_value () =
  let open Monad in
  let value = 42 in
  let program =
    let* v = get in
    set @@ (1 + v)
  in
  let expected = ((), value)
  and computed = program 41 in
  Alcotest.(check (pair unit int))
    "Should retrieve and set a new value" expected computed
;;

let should_modify_a_value () =
  let value = 42 in
  let program = modify @@ ( + ) 1 in
  let expected = ((), value)
  and computed = program 41 in
  Alcotest.(check (pair unit int)) "Should modify the value" expected computed
;;

let should_set_and_modify_a_value () =
  let open Monad in
  let value = 42 in
  let program =
    let* () = set 41 in
    modify @@ ( + ) 1
  in
  let expected = ((), value)
  and computed = program 0 in
  Alcotest.(check (pair unit int))
    "Should set and modify the value" expected computed
;;

let should_get_set_and_modify_a_value () =
  let open Monad in
  let value = 42 in
  let program =
    let* v = get in
    let* () = set @@ (v + 1) in
    modify @@ ( + ) 1
  in
  let expected = ((), value)
  and computed = program 40 in
  Alcotest.(check (pair unit int))
    "Should get and modify the value" expected computed
;;

let should_set_modify_and_get_a_value () =
  let open Monad in
  let value = 42 in
  let program =
    let* () = set 41 in
    let* () = modify @@ ( + ) 1 in
    get
  in
  let expected = (value, value)
  and computed = program 0 in
  Alcotest.(check (pair int int))
    "Should set modify and get the value" expected computed
;;

let should_get_modify_and_get_a_value () =
  let open Monad in
  let value = 42 in
  let program =
    let* v = get in
    let* () = modify @@ ( + ) v in
    get
  in
  let expected = (value, value)
  and computed = program 21 in
  Alcotest.(check (pair int int))
    "Should get modify and get the value" expected computed
;;

let test_cases =
  let open Alcotest in
  [
    ( "State"
    , [
        test_case "Should retrieve value" `Quick should_get_value
      ; test_case "Should retrieve and set new value" `Quick
          should_get_and_set_a_new_value
      ; test_case "Should modify a value" `Quick should_modify_a_value
      ; test_case "Should set and modify a value" `Quick
          should_set_and_modify_a_value
      ; test_case "Should get and modify a value" `Quick
          should_get_set_and_modify_a_value
      ; test_case "Should set modify and get a value" `Quick
          should_set_modify_and_get_a_value
      ; test_case "Should get modify and get a value" `Quick
          should_get_modify_and_get_a_value
      ] )
  ]
;;
