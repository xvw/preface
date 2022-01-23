exception Invalid_int of string
exception Missing_field of string
exception String_shorter of (string * int)

let err = Preface.Nonempty_list.create

type 'a t = {
    parser : string -> 'a Preface.Validate.t
  ; name : string
}

module Functor = Preface.Make.Functor.Via_map (struct
  type nonrec 'a t = 'a t

  let map f env =
    {
      env with
      parser =
        (fun x ->
          let open Preface.Validation in
          match env.parser x with
          | Invalid x -> Invalid x
          | Valid value -> Valid (f value) )
    }
  ;;
end)

let parser parser name = { parser; name }

let read_int x =
  match int_of_string_opt x with
  | None ->
    let message = err (Invalid_int x) in
    Preface.Validate.invalid message
  | Some x -> Preface.Validation.valid x
;;

let string_with_len potential_len value =
  match potential_len with
  | None -> Preface.Validate.pure value
  | Some x ->
    let len = abs x in
    if String.length value >= len
    then Preface.Validate.pure value
    else Preface.Validate.invalid (err (String_shorter (value, x)))
;;

module Free = Preface.Make.Free_applicative.Over_functor (Functor)

let field name reader = Free.promote (parser reader name)
let string ?len name = field name (string_with_len len)
let int name = field name read_int

module Run = Free.To_applicative (Preface.Validate.Applicative)

module Count = Free.To_monoid (struct
  type t = int

  let neutral = 0
  let combine x y = x + y
end)

module Fields = Free.To_monoid (Preface.List.Monoid (String))

let read raw_user { parser; name } =
  match List.assoc_opt name raw_user with
  | None -> Preface.Validate.invalid @@ err (Missing_field name)
  | Some x -> parser x
;;

module User = struct
  type t = {
      name : string
    ; nickname : string
    ; age : int
  }

  let equal a b =
    String.equal a.name b.name
    && String.equal a.nickname b.nickname
    && Int.equal a.age b.age
  ;;

  let pp ppf { name; nickname; age } =
    Format.fprintf ppf "User{name = %s; nickname = %s; age = %d}" name nickname
      age
  ;;

  let make name nickname age = { name; nickname; age }

  let validate =
    let open Free in
    make <$> string "name" <*> string ~len:3 "nickname" <*> int "age"
  ;;
end

let run input =
  let nt =
    let open Run in
    let transform x = read input x in
    { transform }
  in
  Run.run nt User.validate
;;

let count =
  let nt =
    let open Count in
    let transform _ = 1 in
    { transform }
  in
  Count.run nt User.validate
;;

let fields =
  let nt =
    let open Fields in
    let transform x = [ x.name ] in
    { transform }
  in
  Fields.run nt User.validate
;;

let validated_user_testable =
  Alcotest.testable
    (Preface.Validate.pp User.pp)
    (Preface.Validate.equal User.equal)
;;

let test_with_valid_user () =
  let expected = Preface.Validate.valid (User.make "Antoine" "XHTMLBoy" 37)
  and computed =
    run [ ("age", "37"); ("nickname", "XHTMLBoy"); ("name", "Antoine") ]
  in
  Alcotest.(check validated_user_testable)
    "user should be valid" expected computed
;;

let test_with_invalid_user_missing_name () =
  let expected = Preface.Validate.invalid (err (Missing_field "name"))
  and computed = run [ ("age", "37"); ("nickname", "XHTMLBoy") ] in
  Alcotest.(check validated_user_testable)
    "user should be invalid" expected computed
;;

let test_with_invalid_user_missing_name_and_invalid_age () =
  let expected =
    Preface.(
      Validate.invalid
        Nonempty_list.(Invalid_int "coq" :: Last (Missing_field "name")))
  and computed = run [ ("age", "coq"); ("nickname", "XHTMLBoy") ] in
  Alcotest.(check validated_user_testable)
    "user should be invalid" expected computed
;;

let test_static_analysis_count () =
  Alcotest.(check int) "there is 3 fields" 3 count
;;

let test_static_analysis_fields () =
  Alcotest.(check (list string))
    "there is 3 nammed fields"
    (List.sort String.compare [ "nickname"; "age"; "name" ])
    (List.sort String.compare fields)
;;

let cases =
  let open Alcotest in
  [
    ( "Free Applicative Formlet"
    , [
        test_case "test with valid user" `Quick test_with_valid_user
      ; test_case "test with missing field name" `Quick
          test_with_invalid_user_missing_name
      ; test_case "test with missing field name and invalid age" `Quick
          test_with_invalid_user_missing_name_and_invalid_age
      ; test_case "static analysis: count" `Quick test_static_analysis_count
      ; test_case "static analysis: fields" `Quick test_static_analysis_fields
      ] )
  ]
;;
