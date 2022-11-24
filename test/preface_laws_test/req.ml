module Identity = struct
  type 'a t = 'a Preface.Identity.t

  let pp = Preface.Identity.pp

  let generator subgen =
    QCheck2.Gen.map (fun x -> Preface.Identity.pure x) subgen
  ;;

  let observable subobs =
    let eq = Preface.Identity.equal (QCheck2.Observable.equal subobs) in
    let print =
      Format.asprintf "%a"
        (Preface.Identity.pp
           (Util.pp_of_print @@ QCheck2.Observable.print subobs) )
    in
    let hash x = QCheck2.Observable.hash subobs (Preface.Identity.extract x) in
    QCheck2.Observable.make ~eq ~hash print
  ;;

  let equal = Preface.Identity.equal
end

module List = struct
  type 'a t = 'a list

  let generator x = QCheck2.Gen.list_size (QCheck2.Gen.int_bound 4) x
  let observable x = QCheck2.Observable.list x
  let pp x = Preface.List.pp x
  let equal x = Stdlib.List.equal x
end

module Option = struct
  type 'a t = 'a option

  let generator x = QCheck2.Gen.option x
  let observable x = QCheck2.Observable.option x
  let pp x = Preface.Option.pp x
  let equal x = Stdlib.Option.equal x
end

module Try = struct
  type 'a t = 'a Preface.Try.t

  let generator x = Preface.Qcheck.Util.gen_try x
  let observable x = Preface.Qcheck.Util.obs_try x
  let pp x = Preface.Try.pp x
  let equal x = Preface.Try.equal x
end

module Equivalence = struct
  type 'a t = 'a Preface.Equivalence.t
  type 'a input = 'a * 'a
  type 'a generator = ('a -> 'a -> bool) QCheck2.fun_

  let generator obs = QCheck2.fun2 obs obs QCheck2.Gen.bool
  let input x = QCheck2.Gen.tup2 x x
  let lift f = QCheck2.Fn.apply f
  let run_equality (a, b) l r = Bool.equal (l a b) (r a b)
end

module Predicate = struct
  type 'a input = 'a
  type 'a generator = ('a -> bool) QCheck2.fun_
  type 'a t = 'a Preface.Predicate.t

  let generator obs = QCheck2.fun1 obs QCheck2.Gen.bool
  let input x = x
  let lift f = QCheck2.Fn.apply f
  let run_equality x l r = Bool.equal (l x) (r x)
end

module Pair = struct
  type ('a, 'b) t = ('a, 'b) Preface.Pair.t

  let generator f s = QCheck2.Gen.tup2 f s
  let observable f s = QCheck2.Observable.pair f s
  let pp f s = Preface.Pair.pp f s
  let equal f s = Preface.Pair.equal f s
end

module Fun = struct
  type ('a, 'b) t = ('a, 'b) Preface.Fun.t
  type ('a, 'b) generator = ('a -> 'b) QCheck2.fun_
  type 'a input = 'a
  type 'a output = 'a

  let generator obs gen = QCheck2.fun1 obs gen
  let input x = x
  let lift f = QCheck2.Fn.apply f
  let equal eq x y = eq x y
  let run_equality x eq l r = eq (l x) (r x)
  let run f x = f x
  let run_functional_output f x = f x
  let map_input f x = f x
end

module Mini_yocaml = struct
  type ('a, 'b) t = {
      deps : string list
    ; f : 'a -> 'b Preface.Identity.t
  }

  module Req = struct
    type nonrec ('a, 'b) t = ('a, 'b) t
    type 'a output = 'a Preface.Identity.t

    type ('a, 'b) generator =
      string list * ('a -> 'b Preface.Identity.t) QCheck2.fun_

    type 'a input = 'a

    let generator obs gen =
      QCheck2.(
        Gen.tup2 (List.generator Gen.string) (fun1 obs (Identity.generator gen)) )
    ;;

    let input x = x
    let lift (deps, f) = { deps; f = QCheck2.Fn.apply f }
    let equal eq x y = Preface.Identity.equal eq x y
    let run { f; deps = _ } x = f x

    let run_equality x eq l r =
      List.equal String.equal l.deps r.deps && eq (l.f x) (r.f x)
    ;;

    let run_functional_output f x =
      Preface.Identity.Functor.map (fun f -> f x) f
    ;;

    let map_input f x = f x
  end

  module Category = Preface.Make.Category.Via_id_and_compose (struct
    type nonrec ('a, 'b) t = ('a, 'b) t

    let id = { deps = []; f = Preface.Identity.Monad.return }

    let compose { deps = deps_a; f = f_a } { deps = deps_b; f = f_b } =
      let deps = deps_a @ deps_b
      and f = Preface.Identity.Monad.(f_a <=< f_b) in
      { f; deps }
    ;;
  end)

  module Profunctor = Preface.Make.Profunctor.Via_dimap (struct
    type nonrec ('a, 'b) t = ('a, 'b) t

    let dimap fst snd { deps; f } =
      let f x = Preface.Identity.Monad.(f (fst x) >|= snd) in
      { deps; f }
    ;;
  end)

  module Strong =
    Preface.Make.Strong.Over_profunctor_via_fst
      (Profunctor)
      (struct
        type nonrec ('a, 'b) t = ('a, 'b) t

        let fst { deps; f } =
          {
            deps
          ; f =
              (fun (x, y) ->
                Preface.Identity.Monad.(f x >>= fun r -> return (r, y)) )
          }
        ;;
      end)

  module Choice =
    Preface.Make.Choice.Over_profunctor_via_left
      (Profunctor)
      (struct
        type nonrec ('a, 'b) t = ('a, 'b) t

        let left { deps; f } =
          {
            deps
          ; f =
              (function
              | Stdlib.Either.Left x ->
                Preface.Identity.Functor.map Either.left (f x)
              | Right x -> Preface.Identity.Monad.(map Either.right (return x))
              )
          }
        ;;
      end)

  module Arrow = Preface.Make.Arrow.From_strong_and_category (Strong) (Category)

  module Arrow_choice =
    Preface.Make.Arrow_choice.Over_arrow_with_left
      (Arrow)
      (struct
        type nonrec ('a, 'b) t = ('a, 'b) t

        let left = Choice.left
      end)
end
