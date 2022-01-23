let try_testable a =
  Alcotest.testable
    (Preface.Try.pp (Alcotest.pp a))
    (Preface.Try.equal (Alcotest.equal a))
;;

type 'a effect =
  | Get_home of (string -> 'a)
  | Print of (string * (unit -> 'a))

module Effect = struct
  include Preface.Make.Freer_monad.Over (struct
    type 'a t = 'a effect
  end)

  let get_home = perform (Get_home Fun.id)
  let print message = perform (Print (message, Fun.id))

  let program path =
    let open Monad in
    match path with
    | Some path ->
      let* () = print ("Path is " ^ path) in
      return (Preface.Try.ok ())
    | None ->
      let* home = get_home in
      let* () = print ("Path is " ^ home) in
      return (Preface.Try.ok ())
  ;;
end

exception No_home

let record reference action = reference := !reference @ [ action ]

let happy_path_without_path () =
  let output = ref [] in
  let expected = [ "get_home"; "print Path is /xhtmlboi" ] in
  let result =
    let open Effect in
    run
      {
        handler =
          (fun resume -> function
            | Print (message, k) ->
              let () = record output ("print " ^ message) in
              resume (k ())
            | Get_home k ->
              let () = record output "get_home" in
              resume (k "/xhtmlboi") )
      }
      (program None)
  in
  Alcotest.(check (try_testable unit)) "should be equal" result (Ok ());
  Alcotest.(check (list string)) "should be equal" expected !output
;;

let happy_path_with_path () =
  let output = ref [] in
  let expected = [ "print Path is ./a-path" ] in
  let result =
    let open Effect in
    run
      {
        handler =
          (fun resume -> function
            | Print (message, k) ->
              let () = record output ("print " ^ message) in
              resume (k ())
            | Get_home k ->
              let () = record output "get_home" in
              resume (k "/xhtmlboi") )
      }
      (program (Some "./a-path"))
  in
  Alcotest.(check (try_testable unit)) "should be equal" result (Ok ());
  Alcotest.(check (list string)) "should be equal" expected !output
;;

let unhappy_path_without_path () =
  let output = ref [] in
  let expected = [ "get_home" ] in
  let result =
    let open Effect in
    run
      {
        handler =
          (fun resume -> function
            | Print (message, k) ->
              let () = record output ("print " ^ message) in
              resume (k ())
            | Get_home _ ->
              let () = record output "get_home" in
              Preface.Try.error No_home )
      }
      (program None)
  in
  Alcotest.(check (try_testable unit))
    "should be equal" result
    (Preface.Try.error No_home);
  Alcotest.(check (list string)) "should be equal" expected !output
;;

let cases =
  [
    ( "Freer Monad OS effect with explicit continuation"
    , let open Alcotest in
      [
        test_case "Happy path: perform program without path" `Quick
          happy_path_without_path
      ; test_case "Happy path: perform program with path" `Quick
          happy_path_with_path
      ; test_case "Unhappy path: perform program without path" `Quick
          unhappy_path_without_path
      ] )
  ]
;;
