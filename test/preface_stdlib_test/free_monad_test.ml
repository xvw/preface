open Preface_core.Fun

module ConsoleIO = struct
  type 'a t =
    | Tell of (string * 'a)
    | Ask of (string -> 'a)

  module Functor = Preface_make.Functor.Via_map (struct
    type nonrec 'a t = 'a t

    let map f x =
      match x with
      | Tell (s, k) -> Tell (s, f k)
      | Ask k -> Ask (fun s -> f (k s))
    ;;
  end)
end

module IO = Preface_make.Free_monad.Over (ConsoleIO.Functor)

(** interpreter corner *)
let runConsoleIO output = function
  | ConsoleIO.Tell (s, k) ->
    let _ = print_string @@ "Execute Tell" ^ s
    and _ = output := !output @ [ s ] in
    k
  | ConsoleIO.Ask k ->
    let _ = print_string "Execute ask" in
    k "Alice"
;;

let tell x = IO.liftF (ConsoleIO.Tell (x, ()))

let ask = IO.liftF (ConsoleIO.Ask id)

let write_hello () =
  let program = tell "Hello" in
  let output = ref [] in
  let expected = [ "Hello" ]
  and _ = IO.run (runConsoleIO output) program in
  Alcotest.(check (list string)) "write hello" expected !output
;;

let read_alice () =
  let program = ask in
  let output = ref [] in
  let expected = []
  and _ = IO.run (runConsoleIO output) program in
  Alcotest.(check (list string)) "read alice" expected !output
;;

let read_alice_write_it () =
  let open IO in
  let program = ask >>= tell in
  let output = ref [] in
  let expected = [ "Alice" ]
  and _ = IO.run (runConsoleIO output) program in
  Alcotest.(check (list string)) "read alice and write it" expected !output
;;

let read_alice_write_hello () =
  let open IO in
  let program = ask >>= (fun n -> tell "Hello" >> tell n) in
  let output = ref [] in
  let expected = [ "Alice"; "Hello" ]
  and _ = IO.run (runConsoleIO output) program in
  Alcotest.(check (list string)) "read alice and write hello" expected !output
;;

let test_cases =
  let open Alcotest in
  [
    ( "Free Monad"
    , [
        test_case "write hello" `Quick write_hello
      ; test_case "read alice" `Quick read_alice
      ; test_case "read alice and write it" `Quick read_alice_write_it
      ; test_case "read alice and write hello" `Quick read_alice_write_hello
      ] )
  ]
;;
