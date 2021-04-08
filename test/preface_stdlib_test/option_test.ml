open Preface.Fun
open Preface.Option

let map_scenario_1 () =
  let expected = Some 31
  and computed =
    Some 20
    |> Functor.map succ
    |> Functor.map (fun x -> x + 9)
    |> Functor.map succ
  in
  Alcotest.(check (option int)) "map_scenario_1" expected computed
;;

let map_scenario_2 () =
  let expected = None
  and computed =
    Some 20 |> Functor.map succ |> const None |> Functor.map (fun x -> x + 9)
  in
  Alcotest.(check (option int)) "map_scenario_2" expected computed
;;

let create age name = (age, name)

let is_positive number = if number >= 0 then Some number else None

let is_potential_name name = if String.length name >= 3 then Some name else None

let parallel_validation_1 () =
  let expected = Some (10, "John Doe")
  and computed =
    Applicative.(create <$> is_positive 10 <*> is_potential_name "John Doe")
  in
  Alcotest.(check (option (pair int string)))
    "parallel_validation_1" expected computed
;;

let parallel_validation_2 () =
  let expected = None
  and computed =
    Applicative.(create <$> is_positive (-12) <*> is_potential_name "John Doe")
  in
  Alcotest.(check (option (pair int string)))
    "parallel_validation_2" expected computed
;;

let parallel_validation_3 () =
  let expected = None
  and computed =
    Applicative.(create <$> is_positive 10 <*> is_potential_name "J")
  in
  Alcotest.(check (option (pair int string)))
    "parallel_validation_2" expected computed
;;

let parallel_validation_4 () =
  let expected = None
  and computed =
    Applicative.(create <$> is_positive (-10) <*> is_potential_name "J")
  in
  Alcotest.(check (option (pair int string)))
    "parallel_validation_2" expected computed
;;

let sequential_validation_1 () =
  let expected = Some (10, "John Doe")
  and computed =
    let open Monad in
    is_positive 10 >|= create >>= (fun f -> is_potential_name "John Doe" >|= f)
  in
  Alcotest.(check (option (pair int string)))
    "Sequential_Validation_1" expected computed
;;

let sequential_validation_2 () =
  let expected = None
  and computed =
    let open Monad in
    is_positive (-10)
    >|= create
    >>= (fun f -> is_potential_name "John Doe" >|= f)
  in
  Alcotest.(check (option (pair int string)))
    "Sequential_Validation_2" expected computed
;;

let sequential_validation_3 () =
  let expected = None
  and computed =
    Monad.(is_positive 10 >|= create >>= (fun f -> is_potential_name "J" >|= f))
  in
  Alcotest.(check (option (pair int string)))
    "Sequential_Validation_1" expected computed
;;

let sequential_validation_4 () =
  let expected = None
  and computed =
    let open Monad in
    is_positive (-10) >|= create >>= (fun f -> is_potential_name "J" >|= f)
  in
  Alcotest.(check (option (pair int string)))
    "Sequential_Validation_1" expected computed
;;

let divide_by x y = if x = 0 then None else Some (y / x)

let sequential_computing_1 () =
  let expected = Some 25
  and computed = Monad.(return 48 >|= succ >|= succ >>= divide_by 2) in
  Alcotest.(check (option int)) "Sequential computing 1" expected computed
;;

let sequential_computing_2 () =
  let expected = None
  and computed =
    Monad.(return 48 >|= succ >>= divide_by 0 >|= succ >>= divide_by 2)
  in
  Alcotest.(check (option int)) "Sequential computing 1" expected computed
;;

let fold_map_over_values () =
  let module Prod = Preface_make.Monoid.Via_combine_and_neutral (struct
    type t = int

    let neutral = 1

    let combine = ( * )
  end) in
  let expected = 120
  and computed = Foldable.fold_map (module Prod) int_of_string (Some "120") in
  Alcotest.(check int) "fold_map with success" expected computed
;;

let fold_map_over_empty () =
  let module Prod = Preface_make.Monoid.Via_combine_and_neutral (struct
    type t = int

    let neutral = 1

    let combine = ( * )
  end) in
  let expected = 1
  and computed = Foldable.fold_map (module Prod) int_of_string None in
  Alcotest.(check int) "fold_map with success" expected computed
;;

let cases =
  let open Alcotest in
  [
    ( "Option"
    , [
        test_case "Map scenario 1" `Quick map_scenario_1
      ; test_case "Map scenario 2" `Quick map_scenario_2
      ; test_case "Parallel validation 1" `Quick parallel_validation_1
      ; test_case "Parallel validation 2" `Quick parallel_validation_2
      ; test_case "Parallel validation 3" `Quick parallel_validation_3
      ; test_case "Parallel validation 4" `Quick parallel_validation_4
      ; test_case "Sequential validation 1" `Quick sequential_validation_1
      ; test_case "Sequential validation 2" `Quick sequential_validation_2
      ; test_case "Sequential validation 3" `Quick sequential_validation_3
      ; test_case "Sequential validation 4" `Quick sequential_validation_4
      ; test_case "Sequential computing 1" `Quick sequential_computing_1
      ; test_case "Sequential computing 2" `Quick sequential_computing_2
      ] )
  ]
;;
