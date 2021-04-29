module S = Preface.List.Monoid (String)
module D = Preface.Traced.Over (S)

let dependencies_of = function
  | "preface_make" -> [ "preface_specs" ]
  | "preface_specs" -> [ "preface_core" ]
  | "preface_stdlib" -> [ "preface_make"; "preface_specs"; "preface_core" ]
  | "preface" -> [ "preface_stdlib" ]
  | _ -> []
;;

let f = Preface.List.Foldable.fold_map (module S) dependencies_of

let direct_deps = D.traced f

let deps subject =
  let open D in
  trace subject (direct_deps =>> traces Fun.id) |> List.sort_uniq String.compare
;;

let deps_for_make () =
  let open Alcotest in
  let expected = [ "preface_core"; "preface_specs" ]
  and computed = deps [ "preface_make" ] in
  check (list string) "Should be equal" expected computed
;;

let deps_for_stdlib () =
  let open Alcotest in
  let expected = [ "preface_core"; "preface_make"; "preface_specs" ]
  and computed = deps [ "preface_stdlib" ] in
  check (list string) "Should be equal" expected computed
;;

let deps_for_specs () =
  let open Alcotest in
  let expected = [ "preface_core" ]
  and computed = deps [ "preface_specs" ] in
  check (list string) "Should be equal" expected computed
;;

let deps_for_preface () =
  let open Alcotest in
  let expected =
    [ "preface_core"; "preface_make"; "preface_specs"; "preface_stdlib" ]
  and computed = deps [ "preface" ] in
  check (list string) "Should be equal" expected computed
;;

let cases =
  let open Alcotest in
  [
    ( "Dependencies computing using Traced Comonad"
    , [
        test_case "Deps for preface_make" `Quick deps_for_make
      ; test_case "Deps for preface_stdlib" `Quick deps_for_stdlib
      ; test_case "Deps for preface_specs" `Quick deps_for_specs
      ; test_case "Deps for preface_preface" `Quick deps_for_preface
      ] )
  ]
;;
