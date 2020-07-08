open Preface_stdlib.Stream

module Requirement = struct
  type nonrec 'a t = 'a t

  let name = "Stream"

  let size = 100

  let arbitrary x = Preface_qcheck.Arbitrary.stream x
end

module Hook = struct
  type nonrec 'a t = 'a t

  let apply x = Obj.magic (take 15 x)
end

module Obs = struct
  type nonrec 'a t = 'a t

  let f obs =
    let print x =
      match take 3 x with
      | Error _ -> "<Stream ...>"
      | Ok x ->
        let p =
          String.concat ";" (Stdlib.List.map (QCheck.Observable.print obs) x)
        in
        "<Stream [" ^ p ^ " ...]>"
    in
    let eq x y =
      match (take 10 x, take 10 y) with
      | (Error _, _) | (_, Error _) -> false
      | (Ok x, Ok y) ->
        let e = QCheck.Observable.equal obs in
        Stdlib.List.for_all2 e x y
    in

    let hash x = Hashtbl.hash (take 10 x) in

    QCheck.Observable.make ~eq ~hash print
  ;;
end

module Functor_test =
  Preface_qcheck.Functor.Make_hooked (Functor) (Requirement) (Hook)
    (Preface_qcheck.Sample.Pack)
module Applicative_test =
  Preface_qcheck.Applicative.Make_hooked (Applicative) (Requirement) (Hook)
    (Preface_qcheck.Sample.Pack)
module Monad_test =
  Preface_qcheck.Monad.Make_hooked (Monad) (Requirement) (Hook)
    (Preface_qcheck.Sample.Pack)
module Comonad_test =
  Preface_qcheck.Comonad.Make_hooked (Comonad) (Requirement) (Hook) (Obs)
    (Preface_qcheck.Sample.Pack)

let cases =
  Functor_test.cases
  @ Applicative_test.cases
  @ Monad_test.cases
  @ Comonad_test.cases
;;
