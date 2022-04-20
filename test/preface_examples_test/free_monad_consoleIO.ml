open Preface.Fun

module ConsoleIO = struct
  type 'a t =
    | Tell of (string * (unit -> 'a))
    | Ask of (string * (string -> 'a))

  module Functor = Preface.Make.Functor.Via_map (struct
    type nonrec 'a t = 'a t

    let map f x =
      match x with
      | Tell (s, k) -> Tell (s, fun () -> f (k ()))
      | Ask (s, k) -> Ask (s, fun s -> f (k s))
    ;;
  end)
end

type 'a need_user_interaction =
  | No of 'a
  | Yes

module Need_user_interaction = struct
  include Preface.Make.Monad.Via_return_and_bind (struct
    type 'a t = 'a need_user_interaction

    let return x = No x
    let bind f = function No x -> f x | Yes -> Yes
  end)

  let to_bool = function No _ -> false | Yes -> true
end

module IO = Preface_make.Free_monad.Over_functor (ConsoleIO.Functor)
module NeedIO = IO.To_monad (Need_user_interaction)
module IdIO = IO.To_monad (Preface.Identity.Monad)

let tell x = IO.perform (ConsoleIO.Tell (x, id))
let ask s = IO.perform (ConsoleIO.Ask (s, id))

let idConsoleIO output =
  let transform : type a. a IO.f -> a Preface.Identity.t =
    let open Preface.Identity.Monad in
    function
    | ConsoleIO.Tell (s, k) ->
      let () = output := !output @ [ "Tell " ^ s ] in
      return () >|= k
    | Ask (s, k) ->
      let () = output := !output @ [ "Ask " ^ s ^ "?" ] in
      return s >|= k
  in
  IdIO.{ transform }
;;

let needConsoleIO =
  let transform : type a. a IO.f -> a Need_user_interaction.t =
    let open Need_user_interaction in
    function ConsoleIO.Tell (_, k) -> No () >|= k | Ask (_, _) -> Yes
  in
  NeedIO.{ transform }
;;

let runConsoleIO output = function
  | ConsoleIO.Tell (s, k) ->
    let () = output := !output @ [ "Tell " ^ s ] in
    k ()
  | ConsoleIO.Ask (s, k) ->
    let () = output := !output @ [ "Ask " ^ s ^ "?" ] in
    k s
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

let read_alice_write_hello_alice_using_id_monad () =
  let open IO in
  let program =
    let* name = ask "Alice" in
    let* () = tell "Hello" in
    tell name
  in
  let output = ref [] in
  let expected = [ "Ask Alice?"; "Tell Hello"; "Tell Alice" ]
  and () = IdIO.run (idConsoleIO output) program |> Preface.Identity.extract in
  Alcotest.(check (list string))
    "read Alice and write Hello Alice" expected !output
;;

let read_alice_write_hello_alice_using_need_interaction_monad () =
  let open IO in
  let program =
    let* name = ask "Alice" in
    let* () = tell "Hello" in
    tell name
  in
  let expected = true
  and computed =
    NeedIO.run needConsoleIO program |> Need_user_interaction.to_bool
  in
  Alcotest.(check bool) "read Alice and write Hello Alice" expected computed
;;

let tell_hello_world_using_need_interaction_monad () =
  let open IO in
  let program =
    let* () = tell "Hello" in
    tell "World"
  in

  let expected = false
  and computed =
    NeedIO.run needConsoleIO program |> Need_user_interaction.to_bool
  in
  Alcotest.(check bool) "write Hello and write World" expected computed
;;

let cases =
  let open Alcotest in
  [
    ( "Free Monad console IO"
    , [
        test_case "write hello" `Quick write_hello
      ; test_case "write hello alice" `Quick write_hello_alice
      ; test_case "read alice" `Quick read_alice
      ; test_case "read alice twice" `Quick read_alice_and_wonderland
      ; test_case "read alice and write it" `Quick read_alice_write_it
      ; test_case "read alice and write hello" `Quick
          read_alice_write_hello_alice
      ; test_case "read alice and write hello using id monad" `Quick
          read_alice_write_hello_alice_using_id_monad
      ; test_case "test `need interaction` when interaction is needed" `Quick
          read_alice_write_hello_alice_using_need_interaction_monad
      ; test_case "test `need interaction` when interaction is not needed"
          `Quick tell_hello_world_using_need_interaction_monad
      ] )
  ]
;;
