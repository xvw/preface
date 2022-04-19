open Preface

let try_testable a =
  Alcotest.testable
    (Preface.Try.pp (Alcotest.pp a))
    (Preface.Try.equal (Alcotest.equal a))
;;

exception Invalid_line

type _ eff =
  | Print : string -> unit eff
  | Read : string eff
  | Fail : exn -> 'a eff

module E = struct
  include Make.Freer_monad.Over (struct
    type 'a t = 'a eff
  end)

  let print msg = perform @@ Print msg
  let read = perform Read
  let fail exn = perform @@ Fail exn
end

module E_try = E.To_monad (Try.Monad)

let program =
  let open E in
  let* () = print "Hello" in
  let* line = read in
  if String.length line = 0 then fail Invalid_line else return line
;;

let try_run ?(empty = false) p =
  let handler : type a. (a, 'b) E_try.handle =
   fun continue -> function
    | Print message ->
      let () = print_endline message in
      continue ()
    | Read ->
      let value = if empty then "" else "foo" in
      continue value
    | Fail exn -> Error exn
  in
  E_try.run { handler } p
;;

let test_with_valid_line () =
  let expected = Try.ok "foo"
  and computed = try_run ~empty:false program in
  Alcotest.(check (try_testable string) "should be equal" expected computed)
;;

let test_with_invalid_line () =
  let expected = Try.error Invalid_line
  and computed = try_run ~empty:true program in
  Alcotest.(check (try_testable string) "should be equal" expected computed)
;;

let cases =
  let open Alcotest in
  [
    ( "Freer To Monad"
    , [
        test_case "Program that returns Ok" `Quick test_with_valid_line
      ; test_case "Program that returns Error" `Quick test_with_invalid_line
      ] )
  ]
;;
