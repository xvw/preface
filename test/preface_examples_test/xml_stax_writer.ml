module Xml = struct
  type xml =
    | PCData of string
    | Tag of string * xml
    | Seq of xml * xml
    | Empty
end

module Stax = struct
  type token =
    | Text of string
    | Open of string
    | Close of string

  let pp ppf = function
    | Text s -> Format.fprintf ppf "text %s" s
    | Open s -> Format.fprintf ppf "open %s" s
    | Close s -> Format.fprintf ppf "close %s" s
  ;;

  let equal f d1 d2 =
    match (d1, d2) with
    | (Text t1, Text t2) -> f t1 t2
    | (Open t1, Open t2) -> f t1 t2
    | (Close t1, Close t2) -> f t1 t2
    | _ -> false
  ;;
end

module Monoid = Preface_stdlib.List.Monoid (struct
  type t = Stax.token
end)

module Writer = Preface.Make.Writer.Over (Monoid)

let rec sax_like =
  let open Xml in
  let open Stax in
  let open Writer in
  let open Writer.Monad in
  function
  | PCData s -> tell [ Text s ]
  | Tag (n, x) ->
    let* _ = tell [ Open n ] in
    let* _ = sax_like x in
    let* _ = tell [ Close n ] in
    return ()
  | Seq (x1, x2) ->
    let* _ = sax_like x1 in
    let* _ = sax_like x2 in
    return ()
  | Empty -> return ()
;;

let sax = Alcotest.testable Stax.pp (Stax.equal ( = ))

let should_transform_a_pcdata () =
  let open Writer in
  let expected = Stax.[ Text "Hello World" ]
  and (_, computed) = run (sax_like Xml.(PCData "Hello World")) in
  Alcotest.(check (list sax)) "transform_a_pcdata" expected computed
;;

let should_transform_a_tag () =
  let open Writer in
  let expected = Stax.[ Open "A"; Close "A" ]
  and (_, computed) = run (sax_like Xml.(Tag ("A", Empty))) in
  Alcotest.(check (list sax)) "transform_a_tag" expected computed
;;

let should_transform_a_sequence () =
  let open Writer in
  let expected = Stax.[ Open "A"; Close "A"; Text "Hello World" ]
  and (_, computed) =
    run (sax_like Xml.(Seq (Tag ("A", Empty), PCData "Hello World")))
  in
  Alcotest.(check (list sax)) "transform_a_sequence" expected computed
;;

let should_transform_empty () =
  let open Writer in
  let expected = []
  and (_, computed) = run (sax_like Xml.Empty) in
  Alcotest.(check (list sax)) "transform_empty" expected computed
;;

let cases =
  let open Alcotest in
  [
    ( "Xml to Stax reader"
    , [
        test_case "Should transform a pcdata" `Quick should_transform_a_pcdata
      ; test_case "Should transform a tag" `Quick should_transform_a_tag
      ; test_case "Should transform a sequence" `Quick
          should_transform_a_sequence
      ; test_case "Should transform empty" `Quick should_transform_empty
      ] )
  ]
;;
