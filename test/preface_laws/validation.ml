module Error = Preface_stdlib.Nonempty_list.Semigroup (struct
  type t = Preface_stdlib.Exn.t
end)

module Req = struct
  type ('a, 'b) t = ('a, 'b) Preface_stdlib.Validation.t

  let arbitrary x y = Preface_qcheck.Arbitrary.validation x y
  let observable x y = Preface_qcheck.Observable.validation x y
  let equal x y = Preface_stdlib.Validation.equal x y
end

module Req_with_exn = struct
  type 'a t = ('a, Error.t) Preface_stdlib.Validation.t

  let arbitrary x =
    Preface_qcheck.Arbitrary.validation x
      Preface_qcheck.Arbitrary.(nonempty_list (exn ()))
  ;;

  let observable x =
    Preface_qcheck.Observable.validation x
      Preface_qcheck.Observable.(nonempty_list exn)
  ;;

  let equal f =
    Preface_stdlib.Validation.equal f
      (Preface_stdlib.Nonempty_list.equal Preface_stdlib.Exn.equal)
  ;;
end

module Functor =
  Preface_laws_pbt.Functor.Cases
    (Preface_stdlib.Validation.Functor (Error)) (Req_with_exn)
    (Preface_qcheck.Sample.Pack1)

module Alt =
  Preface_laws.Alt.Semigroup_cases
    (Preface_stdlib.Validation.Alt (Error)) (Req_with_exn)
    (Preface_qcheck.Sample.Pack1)

module Applicative =
  Preface_laws_pbt.Applicative.Cases
    (Preface_stdlib.Validation.Applicative (Error)) (Req_with_exn)
    (Preface_qcheck.Sample.Pack1)

module Monad =
  Preface_laws_pbt.Monad.Cases
    (Preface_stdlib.Validation.Monad (Error)) (Req_with_exn)
    (Preface_qcheck.Sample.Pack1)

module Bifunctor =
  Preface_laws_pbt.Bifunctor.Cases (Preface_stdlib.Validation.Bifunctor) (Req)
    (Preface_qcheck.Sample.Pack1)

module Selective =
  Preface_laws_pbt.Selective.Cases
    (Preface_stdlib.Validation.Selective (Error)) (Req_with_exn)
    (Preface_qcheck.Sample.Pack1)

let cases n =
  [
    ( "Validation (with error nonempty list as Error part) Functor Laws"
    , Functor.cases n )
  ; ( "Validation (with error nonempty list as Error part) Alt Semigroup Laws"
    , Alt.cases n )
  ; ( "Validation (with error nonempty list as Error part) Applicative Laws"
    , Applicative.cases n )
  ; ( "Validation (with error nonempty list as Error part) Monad Laws"
    , Monad.cases n )
  ; ("Validation Bifunctor Laws", Bifunctor.cases n)
  ; ( "Validation Selective (with error nonempty list as Error part) Laws"
    , Selective.cases n )
  ]
;;
