module Req = struct
  type ('a, 'b) t = ('a, 'b) Preface_stdlib.Tuple.t

  let arbitrary x y = Preface_qcheck.Arbitrary.pair x y

  let observable x y = Preface_qcheck.Observable.pair x y

  let equal x y = Preface_stdlib.Tuple.equal x y
end

module Bifunctor =
  Preface_laws.Bifunctor.Cases (Preface_stdlib.Tuple.Bifunctor) (Req)
    (Preface_qcheck.Sample.Pack1)

let cases n = [ ("Tuple Bifunctor Laws", Bifunctor.cases n) ]
