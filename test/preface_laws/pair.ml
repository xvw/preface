module Req = struct
  type ('a, 'b) t = ('a, 'b) Preface_stdlib.Pair.t

  let arbitrary x y = Preface_qcheck.Arbitrary.pair x y
  let observable x y = Preface_qcheck.Observable.pair x y
  let equal x y = Preface_stdlib.Pair.equal x y
end

module Bifunctor =
  Preface_laws_pbt.Bifunctor.Cases (Preface_stdlib.Pair.Bifunctor) (Req)
    (Preface_qcheck.Sample.Pack1)

let cases n = [ ("Pair Bifunctor Laws", Bifunctor.cases n) ]
