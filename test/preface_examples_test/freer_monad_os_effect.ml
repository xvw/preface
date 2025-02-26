let try_testable a =
  Alcotest.testable
    (Preface.Try.pp (Alcotest.pp a))
    (Preface.Try.equal (Alcotest.equal a))
;;

type 'a an_effect =
  | Get_home : string an_effect
  | Print : string -> unit an_effect

module Effect = struct
  include Preface.Make.Freer_monad.Over (struct
    type 'a t = 'a an_effect
  end)

  let get_home = perform Get_home
  let print message = perform (Print message)

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
          (fun resume an_effect ->
            let f : type b. (b -> 'a) -> b an_effect -> 'a =
             fun resume -> function
              | Print message ->
                let () = record output ("print " ^ message) in
                resume ()
              | Get_home ->
                let () = record output "get_home" in
                resume "/xhtmlboi"
            in
            f resume an_effect )
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
          (fun resume an_effect ->
            let f : type b. (b -> 'a) -> b an_effect -> 'a =
             fun resume -> function
              | Print message ->
                let () = record output ("print " ^ message) in
                resume ()
              | Get_home ->
                let () = record output "get_home" in
                resume "/xhtmlboi"
            in
            f resume an_effect )
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
          (fun resume an_effect ->
            let f : type b. (b -> 'a) -> b an_effect -> 'a =
             fun resume -> function
              | Print message ->
                let () = record output ("print " ^ message) in
                resume ()
              | Get_home ->
                let () = record output "get_home" in
                Preface.Try.error No_home
            in
            f resume an_effect )
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
    ( "Freer Monad OS effect"
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
