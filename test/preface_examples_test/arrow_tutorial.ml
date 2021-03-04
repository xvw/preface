(* 
   Tutorial extracted from [The Arrow Tutorial]:
   https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial 
 *)

(* Some helper *)
let uncurry f (x, y) = f x y

(* Definition of the Circuit as an arrow *)
module Circuit = struct
  type ('a, 'b) t = { eval : 'a -> ('a, 'b) t * 'b }

  module Category = Preface.Make.Category.Via_id_and_compose (struct
    type nonrec ('a, 'b) t = ('a, 'b) t

    let rec id = { eval = (fun x -> (id, x)) }

    let rec compose c2 c1 =
      {
        eval =
          (fun x ->
            let (circuit1, b) = c1.eval x in
            let (circuit2, c) = c2.eval b in
            (compose circuit2 circuit1, c))
      }
    ;;
  end)

  module Arrow =
    Preface.Make.Arrow.Over_category_and_via_arrow_and_fst
      (Category)
      (struct
        type nonrec ('a, 'b) t = ('a, 'b) t

        let rec arrow f = { eval = (fun x -> (arrow f, f x)) }

        let rec fst circuit =
          {
            eval =
              (fun (a, b) ->
                let (cir, c) = circuit.eval a in
                (fst cir, (c, b)))
          }
        ;;
      end)

  module Choice =
    Preface.Make.Arrow_choice.Over_arrow_with_left
      (Arrow)
      (struct
        type nonrec ('a, 'b) t = ('a, 'b) t

        let rec left origin =
          {
            eval =
              (function
              | Either.Left b ->
                let (circuit', c) = origin.eval b in
                (left circuit', Either.Left c)
              | Either.Right d -> (left origin, Either.Right d))
          }
        ;;
      end)

  let rec run circuit = function
    | [] -> []
    | x :: xs ->
      let (cir, a) = circuit.eval x in
      a :: run cir xs
  ;;

  let rec fold acc f =
    {
      eval =
        (fun input ->
          let (output, aux) = f input acc in
          (fold aux f, output))
    }
  ;;

  let reduce acc f =
    fold acc (fun a b ->
        let aux = f a b in
        (aux, aux))
  ;;
end

let total = Circuit.reduce 0 ( + )

let total_float = Circuit.reduce 0. ( +. )

let total_test () =
  let result = Circuit.run total [ 1; 0; 1; 0; 0; 2 ]
  and expected = [ 1; 1; 2; 2; 2; 4 ] in
  Alcotest.(check (list int)) "Lists should be equals" expected result
;;

let avg_proc_test () =
  let circuit =
    let open Circuit.Arrow in
    total_float &&& Fun.const 1. ^>> total_float >>> arrow (uncurry ( /. ))
  in
  let result = Circuit.run circuit [ 0.; 10.; 7.; 8. ]
  and expected = [ 0.; 5.; 5.66667; 6.25 ] in
  Alcotest.(check (list (float 1.))) "Lists should be equals" expected result
;;

let one_shot_test () =
  let one_shot = Circuit.fold true (fun _ acc -> (acc, false)) in
  let result = Circuit.run one_shot [ 1; 2; 3; 4; 5 ]
  and expected = [ true; false; false; false; false ] in
  Alcotest.(check (list bool)) "Lists should be equals" expected result
;;

let delayed_echo_test () =
  let delayed_echo = Circuit.fold false (fun x y -> (y, x)) in
  let result = Circuit.run delayed_echo [ true; false; false; false; true ]
  and expected = [ false; true; false; false; false ] in
  Alcotest.(check (list bool)) "List should be equals" expected result
;;

let cases =
  let open Alcotest in
  [
    ( "The Arrow tutorial"
    , [
        test_case "run circuit using total" `Quick total_test
      ; test_case "run circuit which compute average" `Quick avg_proc_test
      ; test_case "run circuit which return true first and false after" `Quick
          one_shot_test
      ; test_case
          "run circuit wich store a value and returns it when it get a new one"
          `Quick delayed_echo_test
      ] )
  ]
;;
