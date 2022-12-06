module Exn = struct
  type t = exn

  let generator = Preface.Qcheck.Util.gen_exn
  let observable = Preface.Qcheck.Util.obs_exn
  let pp = Preface.Qcheck.Util.pp_exn
  let equal = Preface.Qcheck.Util.equal_exn
end

module Over
    (M : Preface.Specs.MONOID)
    (T : Preface.Qcheck.Model.T0 with type t = M.t) =
struct
  include Preface.Approximation.Over (M)

  module Req = struct
    type nonrec 'a t = 'a t

    let pp _ ppf (Over x) = Format.fprintf ppf "Over %a" T.pp x
    let equal _ (Over a) (Over b) = T.equal a b
    let generator a = QCheck2.Gen.map Applicative.pure a

    let observable _ =
      let print (Over x) = Format.asprintf "Over %a" T.pp x in
      let eq (Over x) (Over y) = T.equal x y in
      QCheck2.Observable.make ~eq print
    ;;
  end
end

module Under
    (M : Preface.Specs.MONOID)
    (T : Preface.Qcheck.Model.T0 with type t = M.t) =
struct
  include Preface.Approximation.Under (M)

  module Req = struct
    type nonrec 'a t = 'a t

    let pp _ ppf (Under x) = Format.fprintf ppf "Under %a" T.pp x
    let equal _ (Under a) (Under b) = T.equal a b
    let generator a = QCheck2.Gen.map Applicative.pure a

    let observable _ =
      let print (Under x) = Format.asprintf "Under %a" T.pp x in
      let eq (Under x) (Under y) = T.equal x y in
      QCheck2.Observable.make ~eq print
    ;;
  end
end

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

  module Mono (T : Preface.Qcheck.Model.T0) = struct
    type t = T.t list

    let generator = generator T.generator
    let observable = observable T.observable
    let pp = pp T.pp
    let equal = equal T.equal
  end
end

module Seq = struct
  type 'a t = 'a Stdlib.Seq.t

  let generator x =
    let open QCheck2.Gen in
    let+ xs = list_size (int_bound 4) x in
    Stdlib.List.to_seq xs
  ;;

  let observable subobs =
    let eq = Preface.Seq.equal (QCheck2.Observable.equal subobs) in
    let print =
      Format.asprintf "%a"
        (Preface.Seq.pp (Util.pp_of_print @@ QCheck2.Observable.print subobs))
    in
    let hash x =
      Stdlib.Seq.fold_left
        (fun acc x -> Hashtbl.seeded_hash acc (QCheck2.Observable.hash subobs x))
        0x42 x
    in
    QCheck2.Observable.make ~eq ~hash print
  ;;

  let pp x = Preface.Seq.pp x
  let equal x = Preface.Seq.equal x

  module Mono (T : Preface.Qcheck.Model.T0) = struct
    type t = T.t Stdlib.Seq.t

    let generator = generator T.generator
    let observable = observable T.observable
    let pp = pp T.pp
    let equal = equal T.equal
  end
end

module Nonempty_list = struct
  type 'a t = 'a Preface.Nonempty_list.t

  let generator x =
    let open QCheck2.Gen in
    let* xs = list_size (int_bound 4) x in
    let+ x = x in
    Stdlib.List.fold_left
      Preface.Nonempty_list.(fun acc x -> x :: acc)
      (Preface.Nonempty_list.create x)
      xs
  ;;

  let observable subobs =
    let eq = Preface.Nonempty_list.equal (QCheck2.Observable.equal subobs) in
    let print =
      Format.asprintf "%a"
        (Preface.Nonempty_list.pp
           (Util.pp_of_print @@ QCheck2.Observable.print subobs) )
    in
    let hash x =
      Preface.Nonempty_list.fold_left
        (fun acc x -> Hashtbl.seeded_hash acc (QCheck2.Observable.hash subobs x))
        0x42 x
    in
    QCheck2.Observable.make ~eq ~hash print
  ;;

  let pp x = Preface.Nonempty_list.pp x
  let equal x = Preface.Nonempty_list.equal x

  module Mono (T : Preface.Qcheck.Model.T0) = struct
    type t = T.t Preface.Nonempty_list.t

    let generator = generator T.generator
    let observable = observable T.observable
    let pp = pp T.pp
    let equal = equal T.equal
  end
end

module Option = struct
  type 'a t = 'a option

  let generator x = QCheck2.Gen.option x
  let observable x = QCheck2.Observable.option x
  let pp x = Preface.Option.pp x
  let equal x = Stdlib.Option.equal x
end

module Stream = struct
  type 'a t = 'a Preface.Stream.t

  let fuel = 7

  let generator x =
    let open QCheck2.Gen in
    let rec f x () =
      let v = generate1 x in
      Preface.Stream.(stream v (lazy (f x ())))
    in
    pure (f x ())
  ;;

  let equal f a b =
    let l = Preface.Stream.take fuel a
    and r = Preface.Stream.take fuel b in
    Preface.Try.equal (List.equal f) l r
  ;;

  let pp f ppf x =
    Format.fprintf ppf "%a"
      (Preface.Try.pp (Preface.List.pp f))
      (Preface.Stream.take fuel x)
  ;;

  let observable subobs =
    let open QCheck2 in
    let print =
      Format.asprintf "%a" (pp (Util.pp_of_print @@ Observable.print subobs))
    and eq = equal (Observable.equal subobs)
    and hash x =
      let r = Preface.Stream.take fuel x in
      let e = Preface.Qcheck.Util.obs_try (Observable.list subobs) in
      Observable.hash e r
    in
    Observable.make ~hash ~eq print
  ;;
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

module Either = struct
  type ('a, 'b) t = ('a, 'b) Preface.Either.t

  let generator l r = Preface.Qcheck.Util.gen_either l r
  let observable l r = Preface.Qcheck.Util.obs_either l r
  let pp l r = Preface.Qcheck.Util.pp_either l r
  let equal l r = Preface.Qcheck.Util.equal_either l r

  module Mono (T : Preface.Qcheck.Model.T0) = struct
    type 'a t = (T.t, 'a) Preface.Either.t

    let generator r = generator T.generator r
    let observable r = observable T.observable r
    let pp r = pp T.pp r
    let equal r = equal T.equal r
  end
end

module Validation = struct
  type ('a, 'b) t = ('a, 'b) Preface.Validation.t

  let pp = Preface.Validation.pp
  let equal = Preface.Validation.equal

  let generator a b =
    let open QCheck2.Gen in
    frequency
      [
        (7, a >|= Preface.Validation.valid)
      ; (3, b >|= Preface.Validation.invalid)
      ]
  ;;

  let observable a b =
    let open QCheck2.Observable in
    let hash = function
      | Preface.Validation.Valid x -> Hashtbl.seeded_hash 42 (hash a x)
      | Preface.Validation.Invalid x -> Hashtbl.seeded_hash 43 (hash b x)
    and print =
      Format.asprintf "%a"
        (pp (Util.pp_of_print (print a)) (Util.pp_of_print (print b)))
    and eq = Preface.Validation.equal (equal a) (equal b) in
    make ~eq ~hash print
  ;;

  module Mono (T : Preface.Qcheck.Model.T0) = struct
    type 'a t = ('a, T.t) Preface.Validation.t

    let pp r = pp r T.pp
    let equal r = equal r T.equal
    let generator r = generator r T.generator
    let observable r = observable r T.observable
  end
end

module Validate = Validation.Mono (Nonempty_list.Mono (Exn))

module Result = struct
  type ('a, 'b) t = ('a, 'b) Preface.Result.t

  let generator l r = Preface.Qcheck.Util.gen_result l r
  let observable l r = Preface.Qcheck.Util.obs_result l r
  let pp l r = Preface.Qcheck.Util.pp_result l r
  let equal l r = Stdlib.Result.equal ~ok:l ~error:r

  module Mono (T : Preface.Qcheck.Model.T0) = struct
    type 'a t = ('a, T.t) Preface.Result.t

    let generator r = generator r T.generator
    let observable r = observable r T.observable
    let pp r = pp r T.pp
    let equal r = equal r T.equal
  end
end

module Try = Result.Mono (Exn)

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

module Continuation = struct
  type 'a t = 'a Preface.Continuation.t

  let pp f ppf x =
    Format.fprintf ppf "<continuation -> %a>" f
      (x.Preface.Continuation.run (fun x -> x))
  ;;

  let equal f x y =
    let a = x.Preface.Continuation.run (fun x -> x)
    and b = y.Preface.Continuation.run (fun x -> x) in
    f a b
  ;;

  let generator x =
    let open QCheck2.Gen in
    let+ x = x in
    Preface.Continuation.pure x
  ;;

  let observable obs =
    let cont_equal = equal in
    let open QCheck2.Observable in
    let print = Format.asprintf "%a" (pp (Util.pp_of_print (print obs))) in
    let hash x =
      let a = x.Preface.Continuation.run (fun x -> x) in
      hash obs a
    in
    let eq = cont_equal (equal obs) in
    make ~eq ~hash print
  ;;
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
                Preface.Identity.Functor.map Stdlib.Either.left (f x)
              | Right x ->
                Preface.Identity.Monad.(map Stdlib.Either.right (return x)) )
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
