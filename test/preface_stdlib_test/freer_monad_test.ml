open Preface_core.Fun

module ConsoleIO = struct
  type 'a t =
    | Tell of (string * (unit -> 'a))
    | Ask of (string * (string -> 'a))
end

module IO = Preface_make.Freer_monad.Via_type (ConsoleIO)

let tell x = IO.perform (ConsoleIO.Tell (x, id))

let ask s = IO.perform (ConsoleIO.Ask (s, id))

let runConsoleIO output = function
  | ConsoleIO.Tell (s, k) ->
    let () = output := !output @ [ "Tell " ^ s ] in
    k ()
  | ConsoleIO.Ask (s, k) ->
    let () = output := !output @ [ "Ask " ^ s ^ "?" ] in
    k s
;;

let runConsole output =
  let open IO in
  let i c = runConsoleIO output c in
  { interpreter = i }
;;

let write_hello () =
  let program = tell "Hello" in
  let output = ref [] in
  let expected = [ "Tell Hello" ]
  and _ = IO.run (runConsole output) program in
  Alcotest.(check (list string)) "write hello" expected !output
;;

let write_hello_alice () =
  let open IO in
  let program = tell "Hello" >> tell "Alice" in
  let output = ref [] in
  let expected = [ "Tell Hello"; "Tell Alice" ]
  and _ = IO.run (runConsole output) program in
  Alcotest.(check (list string)) "write Hello Alice" expected !output
;;

let read_alice () =
  let program = ask "Alice" in
  let output = ref [] in
  let expected = [ "Ask Alice?" ]
  and _ = IO.run (runConsole output) program in
  Alcotest.(check (list string)) "read alice" expected !output
;;

let read_alice_and_wonderland () =
  let open IO in
  let program =
    let* _ = ask "Alice" in
    ask "Wonderland"
  in
  let output = ref [] in
  let expected = [ "Ask Alice?"; "Ask Wonderland?" ]
  and _ = IO.run (runConsole output) program in
  Alcotest.(check (list string)) "read alice" expected !output
;;

let read_alice_write_it () =
  let open IO in
  let program =
    let* name = ask "Alice" in
    tell name
  in
  let output = ref [] in
  let expected = [ "Ask Alice?"; "Tell Alice" ]
  and _ = IO.run (runConsole output) program in
  Alcotest.(check (list string)) "read alice and write it" expected !output
;;

let read_alice_write_hello_alice () =
  let open IO in
  let program =
    let* name = ask "Alice" in
    let* () = tell "Hello" in
    tell name
  in
  let output = ref [] in
  let expected = [ "Ask Alice?"; "Tell Hello"; "Tell Alice" ]
  and _ = IO.run (runConsole output) program in
  Alcotest.(check (list string))
    "read Alice and write Hello Alice" expected !output
;;

let test_cases =
  let open Alcotest in
  [
    ( "Freer Monad"
    , [
        test_case "write hello" `Quick write_hello
      ; test_case "write hello alice" `Quick write_hello_alice
      ; test_case "read alice" `Quick read_alice
      ; test_case "read alice twice" `Quick read_alice_and_wonderland
      ; test_case "read alice and write it" `Quick read_alice_write_it
      ; test_case "read alice and write hello" `Quick
          read_alice_write_hello_alice
      ] )
  ]
;;
