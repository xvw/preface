module Template = struct
  type template_item =
    | Const of string
    | Var of string

  type template = template_item list

  module Bindings = Map.Make (String)

  module Reader = Preface_stdlib.Reader.Over (struct
    type t = string Bindings.t
  end)

  let transform_item =
    let open Reader in
    let open Reader.Monad in
    let open Bindings in
    function
    | Const s -> return s
    | Var s ->
      let* env = ask in
      return (if mem s env then find s env else "N/A")
  ;;

  let rec transform =
    let open Reader.Monad in
    function
    | [] -> return ""
    | a :: l ->
      let* ta = transform_item a in
      let* tl = transform l in
      return (ta ^ tl)
  ;;

  let should_transform_constant () =
    let expected = "Hello"
    and computed = Reader.run (transform [ Const "Hello" ]) Bindings.empty in
    Alcotest.(check string) "transform_constant" expected computed
  ;;

  let should_transform_variable () =
    let expected = "Alice"
    and computed =
      Reader.run (transform [ Var "name" ]) (Bindings.singleton "name" "Alice")
    in
    Alcotest.(check string) "transform_variable" expected computed
  ;;

  let should_not_transform_variable () =
    let expected = "N/A"
    and computed = Reader.run (transform [ Var "name" ]) Bindings.empty in
    Alcotest.(check string) "not_transform_variable" expected computed
  ;;

  let should_transform_constant_and_variable () =
    let expected = "Hello, Alice"
    and computed =
      Reader.run
        (transform [ Const "Hello"; Const ", "; Var "name" ])
        (Bindings.singleton "name" "Alice")
    in
    Alcotest.(check string) "transform_constant_and_variable" expected computed
  ;;
end

module DeBruijn = struct
  module Lambda = struct
    type t =
      | App of t * t
      | Abs of string * t
      | Var of string
  end

  module DeBruijn = struct
    type t =
      | App of t * t
      | Abs of t
      | Var of int

    let rec pp ppf = function
      | App (t1, t2) -> Format.fprintf ppf "%a (%a)" pp t1 pp t2
      | Abs t -> Format.fprintf ppf "{ %a }" pp t
      | Var i -> Format.fprintf ppf "%i" i
    ;;

    let rec eq f d1 d2 =
      match (d1, d2) with
      | (App (d11, d12), App (d21, d22)) -> eq f d11 d21 && eq f d12 d22
      | (Abs d11, Abs d22) -> eq f d11 d22
      | (Var i1, Var i2) -> f i1 i2
      | _ -> false
    ;;
  end

  let lookup n e =
    let rec lookup i = function
      | [] -> None
      | n' :: _ when n' = n -> Some i
      | _ :: e -> lookup (i + 1) e
    in
    lookup 1 e
  ;;

  module Reader = Preface_stdlib.Reader.Over (struct
    type t = string list
  end)

  module Try = Preface_stdlib.Try

  exception FreeVariable of string

  let rec transform =
    let open Reader in
    let open Reader.Monad in
    function
    | Lambda.App (f, a) ->
      let* tf = transform f in
      let* ta = transform a in
      return Try.Applicative.((fun f a -> DeBruijn.App (f, a)) <$> tf <*> ta)
    | Lambda.Abs (n, t) ->
      let* tf = local (fun e -> n :: e) (transform t) in
      return Try.Functor.((fun tf -> DeBruijn.Abs tf) <$> tf)
    | Lambda.Var n -> (
      let* env = ask in
      match lookup n env with
      | None -> return (Error (FreeVariable n))
      | Some i -> return (Ok (DeBruijn.Var i)) )
  ;;

  let debruijn = Alcotest.testable DeBruijn.pp (DeBruijn.eq ( = ))

  let subject a = Alcotest.testable (Try.pp (Alcotest.pp a)) (Try.eq ( = ))

  let should_transform_bound_variable () =
    let expected = Ok DeBruijn.(Var 1)
    and computed = Reader.run (transform Lambda.(Var "n")) [ "n" ] in
    Alcotest.(check (subject debruijn))
      "transform_bind_variable" expected computed
  ;;

  let should_transform_free_variable () =
    let expected = Error (FreeVariable "n")
    and computed = Reader.run (transform Lambda.(Var "n")) [] in
    Alcotest.(check (subject debruijn))
      "transform_free_variable" expected computed
  ;;

  let should_transform_identity_abstraction () =
    let expected = Ok DeBruijn.(Abs (Var 1))
    and computed = Reader.run (transform Lambda.(Abs ("n", Var "n"))) [] in
    Alcotest.(check (subject debruijn))
      "transform_identity_abstraction" expected computed
  ;;

  let should_transform_application () =
    let expected = Ok DeBruijn.(App (Abs (Var 1), Abs (Var 1)))
    and computed =
      Reader.run
        (transform Lambda.(App (Abs ("n", Var "n"), Abs ("n", Var "n"))))
        []
    in
    Alcotest.(check (subject debruijn))
      "transform_application" expected computed
  ;;
end

let test_cases =
  let open Alcotest in
  [
    ( "Reader"
    , [
        test_case "Should transform a constant" `Quick
          Template.should_transform_constant
      ; test_case "Should transform a variable" `Quick
          Template.should_transform_variable
      ; test_case "Should not transform a variable" `Quick
          Template.should_not_transform_variable
      ; test_case "Should not transform a sequence of constants and variable"
          `Quick Template.should_transform_constant_and_variable
      ; test_case "Should transform a bind variable" `Quick
          DeBruijn.should_transform_bound_variable
      ; test_case "Should transform a free variable" `Quick
          DeBruijn.should_transform_free_variable
      ; test_case "Should transform a identity abtraction" `Quick
          DeBruijn.should_transform_identity_abstraction
      ; test_case "Should transform an application" `Quick
          DeBruijn.should_transform_application
      ] )
  ]
;;
