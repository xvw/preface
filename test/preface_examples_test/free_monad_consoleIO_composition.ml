open Preface.Fun

module Tell = struct
  type 'a t = Effect of (string * (unit -> 'a))

  module Functor = Preface.Make.Functor.Via_map (struct
    type nonrec 'a t = 'a t

    let map f x =
      (match x with Effect (s, k) -> Effect (s, (fun () -> f (k ()))))
    ;;
  end)
end

module Ask = struct
  type 'a t = Effect of (string * (string -> 'a))

  module Functor = Preface.Make.Functor.Via_map (struct
    type nonrec 'a t = 'a t

    let map f x =
      (match x with Effect (s, k) -> Effect (s, (fun s -> f (k s))))
    ;;
  end)
end

module ConsoleIO = Preface.Make.Functor.Sum (Ask.Functor) (Tell.Functor)
module IO = Preface_make.Free_monad.Over_functor (ConsoleIO)

let tell x = IO.perform (ConsoleIO.R (Tell.Effect (x, id)))

let ask s = IO.perform (ConsoleIO.L (Ask.Effect (s, id)))

let runConsoleIO output = function
  | ConsoleIO.L (Ask.Effect (s, k)) ->
    let () = output := !output @ [ "Ask " ^ s ^ "?" ] in
    k s
  | ConsoleIO.R (Tell.Effect (s, k)) ->
    let () = output := !output @ [ "Tell " ^ s ] in
    k ()
;;

let write_hello () =
  let program = tell "Hello" in
  let output = ref [] in
  let expected = [ "Tell Hello" ]
  and _ = IO.run (runConsoleIO output) program in
  Alcotest.(check (list string)) "write hello" expected !output
;;

let write_hello_alice () =
  let open IO in
  let program = tell "Hello" >> tell "Alice" in
  let output = ref [] in
  let expected = [ "Tell Hello"; "Tell Alice" ]
  and _ = IO.run (runConsoleIO output) program in
  Alcotest.(check (list string)) "write Hello Alice" expected !output
;;

let read_alice () =
  let program = ask "Alice" in
  let output = ref [] in
  let expected = [ "Ask Alice?" ]
  and _ = IO.run (runConsoleIO output) program in
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
  and _ = IO.run (runConsoleIO output) program in
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
  and _ = IO.run (runConsoleIO output) program in
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
  and _ = IO.run (runConsoleIO output) program in
  Alcotest.(check (list string))
    "read Alice and write Hello Alice" expected !output
;;

let cases =
  let open Alcotest in
  [
    ( "Free Monad console IO using Functor Sum"
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
