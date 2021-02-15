module State = Preface_stdlib.State.Over (Int)

module Req = struct
  type 'a t = 'a State.t

  let print x st =
    let (v, z) = State.run 0 st in
    Format.asprintf "(%s,%d)" (x v) z
  ;;

  let arbitrary x =
    let print = Preface_stdlib.Option.Monad.(x.QCheck.print >|= print) in
    let gen = Preface_qcheck.Gen.state (QCheck.gen x) in
    QCheck.make ?print gen
  ;;

  let equal eq x y =
    let (v, z) = State.run 0 x
    and (v', z') = State.run 0 y in
    eq v v' && Int.equal z z'
  ;;

  let observable x =
    let eq = equal (QCheck.Observable.equal x) in
    QCheck.Observable.make ~eq (print (QCheck.Observable.print x))
  ;;
end

module Functor =
  Preface_laws.Functor.Cases (State.Functor) (Req) (Preface_qcheck.Sample.Pack1)
module Applicative =
  Preface_laws.Applicative.Cases (State.Applicative) (Req)
    (Preface_qcheck.Sample.Pack1)
module Monad =
  Preface_laws.Monad.Cases (State.Monad) (Req) (Preface_qcheck.Sample.Pack1)

let cases n =
  [
    ("State Functor Laws", Functor.cases n)
  ; ("State Applicative Laws", Applicative.cases n)
  ; ("State Monad Laws", Monad.cases n)
  ]
;;
