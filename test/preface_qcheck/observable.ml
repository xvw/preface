open Preface_stdlib
open QCheck

type 'a t = 'a Observable.t

let identity a =
  let eq = Identity.equal (Observable.equal a) in
  let print = Printer.identity (Observable.print a) in
  let hash x =
    let value = Identity.extract x in
    Observable.hash a value
  in
  Observable.make ~eq ~hash print
;;

let either a b =
  let eq = Either.equal (Observable.equal a) (Observable.equal b) in
  let print = Printer.either (Observable.print a) (Observable.print b) in
  Observable.make ~eq print
;;

let result a b =
  let eq = Result.equal (Observable.equal a) (Observable.equal b) in
  let print = Printer.result (Observable.print a) (Observable.print b) in
  Observable.make ~eq print
;;

let validation a b =
  let eq = Validation.equal (Observable.equal a) (Observable.equal b) in
  let print = Printer.validation (Observable.print a) (Observable.print b) in
  Observable.make ~eq print
;;

let exn =
  let eq = Exn.equal in
  let print = Printer.exn in
  Observable.make ~eq print
;;

let try_ a = result a exn

let stream ?(fuel = 200) a =
  let print = Printer.stream (Observable.print a) in
  let eq x y =
    match
      (Preface_stdlib.Stream.take fuel x, Preface_stdlib.Stream.take fuel y)
    with
    | Error _, _ | _, Error _ -> false
    | Ok l, Ok r -> Preface_stdlib.List.equal (Observable.equal a) l r
  in
  let hash x = Hashtbl.hash (Preface_stdlib.Stream.take fuel x) in
  Observable.make ~eq ~hash print
;;

let continuation inner =
  let print = Printer.continuation (Observable.print inner) in
  let eq x y =
    let open Preface_stdlib.Continuation in
    (Observable.equal inner) (x.run Fun.id) (y.run Fun.id)
  in
  Observable.make ~eq print
;;

let nonempty_list inner =
  let print = Printer.nonempty_list (Observable.print inner) in
  let eq x y = Nonempty_list.equal (Observable.equal inner) x y in
  Observable.make ~eq print
;;

let seq inner =
  let print = Printer.seq (Observable.print inner) in
  let eq x y = Seq.equal (Observable.equal inner) x y in
  Observable.make ~eq print
;;

let validate x = validation x (nonempty_list exn)

include (
  QCheck.Observable : module type of QCheck.Observable with type 'a t := 'a t )
