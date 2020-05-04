module N = Preface_core.Nonempty_list

let subject a = Alcotest.(testable (N.pp (pp a)) (N.eq ( = )))

open N

let should_create () =
  let expected = (10, [])
  and computed = create 10 in
  Alcotest.(check (subject int)) "should_create" expected computed
;;

let should_create_from_list_nonempty () =
  let expected = Some (10, [ 11; 12; 13 ])
  and computed = from_list [ 10; 11; 12; 13 ] in
  Alcotest.(check (option (subject int)))
    "should_create_from_list_nonempty" expected computed
;;

let should_create_from_list_singleton () =
  let expected = Some (10, [])
  and computed = from_list [ 10 ] in
  Alcotest.(check (option (subject int)))
    "should_create_from_list_singleton" expected computed
;;

let should_create_from_list_empty () =
  let expected = None
  and computed = from_list [] in
  Alcotest.(check (option (subject int)))
    "should_create_from_list_empty" expected computed
;;

let should_create_list_singleton () =
  let expected = [ 200 ]
  and computed = to_list (create 200) in
  Alcotest.(check (list int)) "should_create_list_singleton" expected computed
;;

let should_extract_head () =
  let expected = 200
  and computed = hd (create 200) in
  Alcotest.(check int) "should_extract_head" expected computed
;;

let should_create_list () =
  let expected = [ 200; 300; 400; 500 ]
  and computed = to_list (cons 200 (cons 300 (cons 400 (create 500)))) in
  Alcotest.(check (list int)) "should_create_list_singleton" expected computed
;;

let should_extract_empty_list () =
  let expected = []
  and computed = tl (create 200) in
  Alcotest.(check (list int)) "should_extract_empty_list" expected computed
;;

let should_extract_list () =
  let expected = [ 300; 400; 500 ]
  and computed = tl (cons 200 (cons 300 (cons 400 (create 500)))) in
  Alcotest.(check (list int)) "should_extract_list" expected computed
;;

let should_have_length_1 () =
  let expected = 1
  and computed = length (create 200) in
  Alcotest.(check int) "should_have_length_1" expected computed
;;

let should_have_length_4 () =
  let expected = 4
  and computed = length (cons 200 (cons 300 (cons 400 (create 500)))) in
  Alcotest.(check int) "should_have_length_4" expected computed
;;

let should_cons () =
  let expected = (200, [ 300; 400; 500 ])
  and computed = cons 200 (cons 300 (cons 400 (create 500))) in
  Alcotest.(check (subject int)) "should_cons" expected computed
;;

let should_rev_singleton () =
  let expected = create 200
  and computed = rev (create 200) in
  Alcotest.(check (subject int)) "should_rev_singleton" expected computed
;;

let should_rev () =
  let expected = (500, [ 400; 300; 200 ])
  and computed = rev (cons 200 (cons 300 (cons 400 (create 500)))) in
  Alcotest.(check (subject int)) "should_rev_singleton" expected computed
;;

let should_iteri_1 () =
  let expected = [ (0, 1); (1, 2); (2, 3) ]
  and computed =
    let x = ref [] in
    let () = iteri (fun i e -> x := !x @ [ (i, e) ]) (1, [ 2; 3 ]) in
    !x
  in
  Alcotest.(check (list (pair int int))) "should_iteri_1" expected computed
;;

let should_iteri_2 () =
  let expected = [ (0, 10) ]
  and computed =
    let x = ref [] in
    let () = iteri (fun i e -> x := !x @ [ (i, e) ]) (10, []) in
    !x
  in
  Alcotest.(check (list (pair int int))) "should_iteri_1" expected computed
;;

let should_iter_1 () =
  let expected = [ (0, 1); (1, 2); (2, 3) ]
  and computed =
    let x = ref [] in
    let () = iter (fun e -> x := !x @ [ (e - 1, e) ]) (1, [ 2; 3 ]) in
    !x
  in
  Alcotest.(check (list (pair int int))) "should_iteri_1" expected computed
;;

let should_iter_2 () =
  let expected = [ (9, 10) ]
  and computed =
    let x = ref [] in
    let () = iter (fun e -> x := !x @ [ (e - 1, e) ]) (10, []) in
    !x
  in
  Alcotest.(check (list (pair int int))) "should_iteri_1" expected computed
;;

let should_mapi_1 () =
  let expected = ((0, "1"), [ (1, "2"); (2, "3"); (3, "4") ])
  and computed = mapi (fun i x -> (i, string_of_int x)) (1, [ 2; 3; 4 ]) in
  Alcotest.(check (subject (pair int string))) "should_mapi_1" expected computed
;;

let should_mapi_2 () =
  let expected = ((0, "1"), [])
  and computed = mapi (fun i x -> (i, string_of_int x)) (1, []) in
  Alcotest.(check (subject (pair int string))) "should_mapi_1" expected computed
;;

let should_map_1 () =
  let expected = ("11", [ "12"; "13"; "14" ])
  and computed = map (fun x -> string_of_int (x + 10)) (1, [ 2; 3; 4 ]) in
  Alcotest.(check (subject string)) "should_mapi_1" expected computed
;;

let should_map_2 () =
  let expected = ("11", [])
  and computed = map (fun x -> string_of_int (x + 10)) (1, []) in
  Alcotest.(check (subject string)) "should_mapi_1" expected computed
;;

let should_fold_left_1 () =
  let expected = "HelloPrefaceOCaml"
  and computed =
    fold_left (fun a x -> a ^ x) "" ("Hello", [ "Preface"; "OCaml" ])
  in
  Alcotest.(check string) "should_fold_left_1" expected computed
;;

let should_fold_left_2 () =
  let expected = "Hello"
  and computed = fold_left (fun a x -> a ^ x) "" ("Hello", []) in
  Alcotest.(check string) "should_fold_left_1" expected computed
;;

let should_fold_right_1 () =
  let expected = "OCamlPrefaceHello"
  and computed =
    fold_right (fun x a -> a ^ x) ("Hello", [ "Preface"; "OCaml" ]) ""
  in
  Alcotest.(check string) "should_fold_left_1" expected computed
;;

let should_fold_right_2 () =
  let expected = "Hello"
  and computed = fold_right (fun x a -> a ^ x) ("Hello", []) "" in
  Alcotest.(check string) "should_fold_left_1" expected computed
;;

let should_flatten_1 () =
  let expected = (1, [ 2; 3; 4; 5; 6 ])
  and computed = flatten ((1, [ 2 ]), [ (3, [ 4 ]); (5, [ 6 ]) ]) in
  Alcotest.(check (subject int)) "should_flatten_1" expected computed
;;

let should_flatten_2 () =
  let expected = (1, [])
  and computed = flatten ((1, []), []) in
  Alcotest.(check (subject int)) "should_flatten_2" expected computed
;;

let test_cases =
  let open Alcotest in
  ( "Nonempty_list"
  , [
      test_case "Create" `Quick should_create
    ; test_case "From_list with element in list" `Quick
        should_create_from_list_nonempty
    ; test_case "From_list with one element in list" `Quick
        should_create_from_list_singleton
    ; test_case "From_list without element in list" `Quick
        should_create_from_list_empty
    ; test_case "Create list from non empty list with one elt" `Quick
        should_create_list_singleton
    ; test_case "Create list from non empty list" `Quick should_create_list
    ; test_case "Should extract head" `Quick should_extract_head
    ; test_case "Should extract empty tail" `Quick should_extract_empty_list
    ; test_case "Should extract tail" `Quick should_extract_list
    ; test_case "Should have length 1" `Quick should_have_length_1
    ; test_case "Should have length 4" `Quick should_have_length_4
    ; test_case "Should cons" `Quick should_cons
    ; test_case "Should rev singleton" `Quick should_rev_singleton
    ; test_case "Should rev" `Quick should_rev
    ; test_case "Should iteri 1" `Quick should_iteri_1
    ; test_case "Should iteri 2" `Quick should_iteri_2
    ; test_case "Should iter 1" `Quick should_iter_1
    ; test_case "Should iter 2" `Quick should_iter_2
    ; test_case "Should mapi 1" `Quick should_mapi_1
    ; test_case "Should mapi 2" `Quick should_mapi_2
    ; test_case "Should map 1" `Quick should_map_1
    ; test_case "Should map 2" `Quick should_map_2
    ; test_case "Should fold left 1" `Quick should_fold_left_1
    ; test_case "Should fold left 2" `Quick should_fold_left_2
    ; test_case "Should fold right 1" `Quick should_fold_right_1
    ; test_case "Should fold right 2" `Quick should_fold_right_2
    ; test_case "Should flatten 1" `Quick should_flatten_1
    ; test_case "Should flatten 2" `Quick should_flatten_2
    ] )
;;
