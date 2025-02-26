type 'a f =
  | Read : string f
  | Write : string -> unit f

type 'a io =
  | IORead : string io
  | IOWrite : string -> unit io

module IO = struct
  include Preface.Make.Freer_monad.Over (struct
    type 'a t = 'a io
  end)

  let get_line = perform IORead
  let put_line s = perform (IOWrite s)

  module Selective =
    Preface.Make.Selective.Over_applicative_via_select
      (Applicative)
      (Preface.Make.Selective.Select_from_monad (Monad))
end

module Freer = struct
  include Preface.Make.Freer_selective.Over (struct
    type nonrec 'a t = 'a f
  end)

  let get_line = promote Read
  let put_line s = promote (Write s)
end

module Run = Freer.To_selective (IO.Selective)

let ping_pong =
  let open Freer in
  when_ ((map (String.equal "ping")) get_line) (put_line "pong")
;;

let free_to_io p =
  let nt =
    let open Run in
    let transform : type a. a f -> a IO.t = function
      | Read -> IO.get_line
      | Write s -> IO.put_line s
    in
    { transform }
  in
  Run.run nt p
;;

let test_when_read_returns_ping () =
  let state = ref [] in
  let _ =
    IO.run
      {
        handler =
          (fun resume an_effect ->
            let f : type b. (b -> 'a) -> b io -> 'a =
             fun resume -> function
              | IOWrite s ->
                let () = state := !state @ [ "Write " ^ s ] in
                resume ()
              | IORead ->
                let () = state := !state @ [ "Read" ] in
                resume "ping"
            in
            f resume an_effect )
      }
      (free_to_io ping_pong)
  in
  Alcotest.(check (list string))
    "The handler should recording pong" [ "Read"; "Write pong" ] !state
;;

let test_when_read_returns_something_else () =
  let state = ref [] in
  let _ =
    IO.run
      {
        handler =
          (fun resume an_effect ->
            let f : type b. (b -> 'a) -> b io -> 'a =
             fun resume -> function
              | IOWrite s ->
                let () = state := !state @ [ "Write " ^ s ] in
                resume ()
              | IORead ->
                let () = state := !state @ [ "Read" ] in
                resume "not_ping"
            in
            f resume an_effect )
      }
      (free_to_io ping_pong)
  in
  Alcotest.(check (list string))
    "The handler should recording pong" [ "Read" ] !state
;;

let cases =
  let open Alcotest in
  [
    ( "Freer Selective Ping Pong"
    , [
        test_case "test reading ping, writing pong" `Quick
          test_when_read_returns_ping
      ; test_case "test reading not_ping, writing nothing" `Quick
          test_when_read_returns_something_else
      ] )
  ]
;;
