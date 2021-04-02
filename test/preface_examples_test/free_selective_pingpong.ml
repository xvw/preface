(* Example taken from 
   [https://github.com/snowleopard/selective/blob/master/examples/Teletype/Rigid.hs] 
*)

type 'a t =
  | Read of (string -> 'a)
  | Write of (string * 'a)

let equal a b =
  match (a, b) with
  | (Read _, Read _) -> true
  | (Write (x, ()), Write (y, ())) -> String.equal x y
  | _ -> false
;;

let pp ppf = function
  | Read _ -> Format.fprintf ppf "[Read]"
  | Write (s, _) -> Format.fprintf ppf "[Write %s]" s
;;

let testable = Alcotest.testable pp equal

type 'a io =
  | IORead : string io
  | IOWrite : string -> unit io

module Functor = Preface.Make.Functor.Via_map (struct
  type nonrec 'a t = 'a t

  let map f = function
    | Write (s, x) -> Write (s, f x)
    | Read r -> Read (fun x -> f (r x))
  ;;
end)

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

module Free = struct
  include Preface.Make.Free_selective.Over_functor (Functor)

  let get_line = promote (Read Fun.id)

  let put_line s = promote (Write (s, ()))
end

module Run = Free.To_selective (IO.Selective)

module Effects = Free.To_monoid (Preface.List.Monoid (struct
  type nonrec t = unit t
end))

let get_effects p =
  let nt =
    let open Effects in
    let transform x = [ Functor.void x ] in
    { transform }
  in
  Effects.run nt p
;;

let free_to_io p =
  let nt =
    let open Run in
    let transform x =
      let open IO.Selective in
      match x with
      | Read f -> f <$> IO.get_line
      | Write (s, x) -> x <$ IO.put_line s
    in
    { transform }
  in
  Run.run nt p
;;

let ping_pong =
  let open Free in
  when_ ((map (String.equal "ping")) get_line) (put_line "pong")
;;

let test_when_read_returns_ping () =
  let state = ref [] in
  let _ =
    IO.run
      {
        handler =
          (fun resume effect ->
            let f : type b. (b -> 'a) -> b io -> 'a =
             fun resume -> function
              | IOWrite s ->
                let () = state := !state @ [ "Write " ^ s ] in
                resume ()
              | IORead ->
                let () = state := !state @ [ "Read" ] in
                resume "ping"
            in
            f resume effect)
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
          (fun resume effect ->
            let f : type b. (b -> 'a) -> b io -> 'a =
             fun resume -> function
              | IOWrite s ->
                let () = state := !state @ [ "Write " ^ s ] in
                resume ()
              | IORead ->
                let () = state := !state @ [ "Read" ] in
                resume "not_ping"
            in
            f resume effect)
      }
      (free_to_io ping_pong)
  in
  Alcotest.(check (list string))
    "The handler should recording pong" [ "Read" ] !state
;;

let test_get_effects () =
  let computed = List.map (Format.asprintf "%a" pp) (get_effects ping_pong) in
  let expected = [ "[Read]"; "[Write pong]" ] in
  Alcotest.(check (list string)) "should be equal" expected computed
;;

let cases =
  let open Alcotest in
  [
    ( "Free Selective Ping Pong"
    , [
        test_case "test reading ping, writing pong" `Quick
          test_when_read_returns_ping
      ; test_case "test reading not_ping, writing nothing" `Quick
          test_when_read_returns_something_else
      ; test_case "static analysis over effects" `Quick test_get_effects
      ] )
  ]
;;
