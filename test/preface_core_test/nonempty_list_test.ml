let nel_testable a =
  Alcotest.testable
    (Preface_core.Nonempty_list.pp (Alcotest.pp a))
    (Preface_core.Nonempty_list.equal (Alcotest.equal a))
;;

open Preface_core.Nonempty_list

let should_create () =
  let expected = Last 10
  and computed = create 10 in
  Alcotest.(check (nel_testable int)) "should_create" expected computed
;;

let should_create_from_list_nonempty () =
  let expected = Some (10 :: 11 :: 12 :: Last 13)
  and computed = from_list [ 10; 11; 12; 13 ] in
  Alcotest.(check (option (nel_testable int)))
    "should_create_from_list_nonempty" expected computed
;;

let should_create_from_list_singleton () =
  let expected = Some (Last 10)
  and computed = from_list [ 10 ] in
  Alcotest.(check (option (nel_testable int)))
    "should_create_from_list_singleton" expected computed
;;

let should_create_from_list_empty () =
  let expected = None
  and computed = from_list [] in
  Alcotest.(check (option (nel_testable int)))
    "should_create_from_list_empty" expected computed
;;

let should_create_list_singleton () =
  let expected = List.[ 200 ]
  and computed = to_list (create 200) in
  Alcotest.(check (list int)) "should_create_list_singleton" expected computed
;;

let should_extract_head () =
  let expected = 200
  and computed = hd (create 200) in
  Alcotest.(check int) "should_extract_head" expected computed
;;

let should_create_list () =
  let expected = List.[ 200; 300; 400; 500 ]
  and computed = to_list (cons 200 (cons 300 (cons 400 (create 500)))) in
  Alcotest.(check (list int)) "should_create_list_singleton" expected computed
;;

let should_extract_empty_list () =
  let expected = None
  and computed = tl (create 200) in
  Alcotest.(check (option (nel_testable int)))
    "should_extract_empty_list" expected computed
;;

let should_extract_list () =
  let expected = Some (300 :: 400 :: Last 500)
  and computed = tl (cons 200 (cons 300 (cons 400 (create 500)))) in
  Alcotest.(check (option (nel_testable int)))
    "should_extract_list" expected computed
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
  let expected = 200 :: 300 :: 400 :: Last 500
  and computed = cons 200 (cons 300 (cons 400 (create 500))) in
  Alcotest.(check (nel_testable int)) "should_cons" expected computed
;;

let should_rev_singleton () =
  let expected = create 200
  and computed = rev (create 200) in
  Alcotest.(check (nel_testable int)) "should_rev_singleton" expected computed
;;

let should_rev () =
  let expected = 500 :: 400 :: 300 :: Last 200
  and computed = rev (cons 200 (cons 300 (cons 400 (create 500)))) in
  Alcotest.(check (nel_testable int)) "should_rev_singleton" expected computed
;;

let should_iteri_1 () =
  let expected = List.[ (0, 1); (1, 2); (2, 3) ]
  and computed =
    let x = ref [] in
    let () = iteri (fun i e -> x := !x @ [ (i, e) ]) (1 :: 2 :: Last 3) in
    !x
  in
  Alcotest.(check (list (pair int int))) "should_iteri_1" expected computed
;;

let should_iteri_2 () =
  let expected = List.[ (0, 10) ]
  and computed =
    let x = ref [] in
    let () = iteri (fun i e -> x := !x @ [ (i, e) ]) (Last 10) in
    !x
  in
  Alcotest.(check (list (pair int int))) "should_iteri_1" expected computed
;;

let should_iter_1 () =
  let expected = List.[ (0, 1); (1, 2); (2, 3) ]
  and computed =
    let x = ref [] in
    let () = iter (fun e -> x := !x @ [ (e - 1, e) ]) (1 :: 2 :: Last 3) in
    !x
  in
  Alcotest.(check (list (pair int int))) "should_iteri_1" expected computed
;;

let should_iter_2 () =
  let expected = List.[ (9, 10) ]
  and computed =
    let x = ref [] in
    let () = iter (fun e -> x := !x @ [ (e - 1, e) ]) (Last 10) in
    !x
  in
  Alcotest.(check (list (pair int int))) "should_iteri_1" expected computed
;;

let should_mapi_1 () =
  let expected = (0, "1") :: (1, "2") :: (2, "3") :: Last (3, "4")
  and computed =
    mapi (fun i x -> (i, string_of_int x)) (1 :: 2 :: 3 :: Last 4)
  in
  Alcotest.(check (nel_testable (pair int string)))
    "should_mapi_1" expected computed
;;

let should_mapi_2 () =
  let expected = Last (0, "1")
  and computed = mapi (fun i x -> (i, string_of_int x)) (Last 1) in
  Alcotest.(check (nel_testable (pair int string)))
    "should_mapi_1" expected computed
;;

let should_map_1 () =
  let expected = "11" :: "12" :: "13" :: Last "14"
  and computed =
    map (fun x -> string_of_int (x + 10)) (1 :: 2 :: 3 :: Last 4)
  in
  Alcotest.(check (nel_testable string)) "should_mapi_1" expected computed
;;

let should_map_2 () =
  let expected = Last "11"
  and computed = map (fun x -> string_of_int (x + 10)) (Last 1) in
  Alcotest.(check (nel_testable string)) "should_mapi_1" expected computed
;;

let should_fold_left_1 () =
  let expected = "HelloPrefaceOCaml"
  and computed =
    fold_left (fun a x -> a ^ x) "" ("Hello" :: "Preface" :: Last "OCaml")
  in
  Alcotest.(check string) "should_fold_left_1" expected computed
;;

let should_fold_left_2 () =
  let expected = "Hello"
  and computed = fold_left (fun a x -> a ^ x) "" (Last "Hello") in
  Alcotest.(check string) "should_fold_left_2" expected computed
;;

let should_fold_right_1 () =
  let expected = "FooOCamlPrefaceHello"
  and computed =
    fold_right (fun x a -> a ^ x) ("Hello" :: "Preface" :: Last "OCaml") "Foo"
  in
  Alcotest.(check string) "should_fold_right_1" expected computed
;;

let should_fold_right_2 () =
  let expected = "BarHello"
  and computed = fold_right (fun x a -> a ^ x) (Last "Hello") "Bar" in
  Alcotest.(check string) "should_fold_right_2" expected computed
;;

let should_append () =
  let expected = 1 :: 2 :: 3 :: 4 :: 5 :: Last 6
  and computed = append (1 :: 2 :: Last 3) (4 :: 5 :: Last 6) in
  Alcotest.(check (nel_testable int)) "should_append" expected computed
;;

let should_rev_append () =
  let expected = List.[ 1; 2; 3; 4; 5; 6 ]
  and computed =
    let l1 = 3 :: 2 :: Last 1
    and l2 = 4 :: 5 :: Last 6 in
    to_list (rev_append l1 l2)
  in
  Alcotest.(check (list int))
    "rev_append_should_according_stdlib" expected computed
;;

let should_flatten_1 () =
  let expected = 1 :: 2 :: 3 :: 4 :: 5 :: Last 6
  and computed =
    flatten ((1 :: Last 2) :: (3 :: Last 4) :: Last (5 :: Last 6))
  in
  Alcotest.(check (nel_testable int)) "should_flatten_1" expected computed
;;

let should_flatten_2 () =
  let expected = Last 1
  and computed = flatten (Last (Last 1)) in
  Alcotest.(check (nel_testable int)) "should_flatten_2" expected computed
;;

let cases =
  let open Alcotest in
  ( "Nonempty_list"
  , let open List in
    [
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
    ; test_case "Should append" `Quick should_append
    ; test_case "Should flatten 1" `Quick should_flatten_1
    ; test_case "Should flatten 2" `Quick should_flatten_2
    ; test_case "Should rev_append" `Quick should_rev_append
    ] )
;;
