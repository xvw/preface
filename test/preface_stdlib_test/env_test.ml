module T = Preface.Env.Over (Int)

module P = Preface.Env.Over (struct
  type t = int * int
end)

let ask_test () =
  let open Alcotest in
  let expected = 1
  and computed = T.ask (T.env 1 "OCaml") in
  check int "Should be equal" expected computed
;;

let asks_test_1 () =
  let open Alcotest in
  let expected = 1
  and computed = P.asks fst (P.env (1, 2) "OCaml") in
  check int "Should be equal" expected computed
;;

let asks_test_2 () =
  let open Alcotest in
  let expected = 2
  and computed = P.asks snd (P.env (1, 2) "OCaml") in
  check int "Should be equal" expected computed
;;

type formatter = {
    padding : int
  ; max : int
  ; char : char
}

module Formatter = Preface.Env.Over (struct
  type t = formatter
end)

let ctx =
  Formatter.env { padding = 2; max = 5; char = '-' } "Hello OCaml Folks!!!"
;;

let get_char env = Formatter.asks (fun x -> x.char) env

let truncate env =
  let len = Formatter.asks (fun x -> x.max) ctx in
  String.sub (Formatter.extract env) 0 len
;;

let padding env =
  let len = Formatter.asks (fun x -> x.padding) env
  and char = Formatter.asks (fun x -> x.char) env in
  let margin = String.make len char in
  margin ^ Formatter.extract env ^ margin
;;

let get_char_is_valid () =
  let open Alcotest in
  let expected = '-'
  and computed = get_char ctx in
  check char "Should be equal" expected computed
;;

let trunc () =
  let open Alcotest in
  let expected = "Hello"
  and computed = truncate ctx in
  check string "Should be equal" expected computed
;;

let padd () =
  let open Alcotest in
  let expected = "--Hello OCaml Folks!!!--"
  and computed = padding ctx in
  check string "Should be equal" expected computed
;;

let pipeline () =
  let open Alcotest in
  let expected = "--Hello--"
  and computed = Formatter.(truncate =>= padding) ctx in
  check string "Should be equal" expected computed
;;

let pipeline2 () =
  let open Alcotest in
  let open Preface.Fun.Infix in
  let expected = "--__Hello__--"
  and computed =
    Formatter.(
      truncate =>= padding % local (fun x -> { x with char = '_' }) =>= padding)
      ctx
  in
  check string "Should be equal" expected computed
;;

let cases =
  let open Alcotest in
  [
    ( "Env"
    , [
        test_case "Test for ask" `Quick ask_test
      ; test_case "Test for asks 1" `Quick asks_test_1
      ; test_case "Test for asks 2" `Quick asks_test_2
      ; test_case "Test for get_char" `Quick get_char_is_valid
      ; test_case "test for truncate settings" `Quick trunc
      ; test_case "test for padding settings" `Quick padd
      ; test_case "test for pipelining padding and truncate settings" `Quick
          pipeline
      ; test_case
          "test for pipelining padding and truncate with mutation on  settings"
          `Quick pipeline2
      ] )
  ]
;;
