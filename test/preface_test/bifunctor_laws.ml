module type BIFUNCTOR = Preface_specs.BIFUNCTOR

module Make
    (Bifunctor : BIFUNCTOR)
    (Req : sig
      type ('a, 'b) t

      val suite_name : string

      val arbitrary :
           'a QCheck.arbitrary
        -> 'b QCheck.arbitrary
        -> ('a, 'b) t QCheck.arbitrary
    end
    with type ('a, 'b) t = ('a, 'b) Bifunctor.t)
    (L : Qcheck_helpers.GENERATOR)
    (R : Qcheck_helpers.GENERATOR)
    (F : Qcheck_helpers.GENERATOR)
    (G : Qcheck_helpers.GENERATOR)
    (H : Qcheck_helpers.GENERATOR)
    (I : Qcheck_helpers.GENERATOR) : Qcheck_helpers.ALCOTEST_SUITE = struct
  let b = Req.arbitrary

  let l = L.arbitrary

  let r = R.arbitrary

  let identity =
    QCheck.Test.make ~count:100 ~name:"bimap id id = id" (b l r) (fun x ->
        let left = Bifunctor.bimap (fun x -> x) (fun x -> x) x
        and right = x in
        left = right)
  ;;

  let first =
    QCheck.Test.make ~count:100 ~name:"fst id = id" (b l r) (fun x ->
        let left = Bifunctor.fst (fun x -> x) x
        and right = x in
        left = right)
  ;;

  let second =
    QCheck.Test.make ~count:100 ~name:"snd id = id" (b l r) (fun x ->
        let left = Bifunctor.snd (fun x -> x) x
        and right = x in
        left = right)
  ;;

  let bimap_fst_snd =
    let f = QCheck.fun1 L.observable F.arbitrary in
    let g = QCheck.fun1 R.observable G.arbitrary in
    let arbitrary = QCheck.triple (b l r) f g in
    QCheck.Test.make ~count:100 ~name:"bimap f g = (fst f) % (snd g)" arbitrary
      (fun (x, f_i, g_i) ->
        let open Preface_core.Fun in
        let f = QCheck.Fn.apply f_i in
        let g = QCheck.Fn.apply g_i in
        let left = Bifunctor.bimap f g x
        and right = (Bifunctor.fst f % Bifunctor.snd g) x in
        left = right)
  ;;

  let bimap_parametricity =
    let g = QCheck.fun1 L.observable F.arbitrary in
    let f = QCheck.fun1 F.observable G.arbitrary in
    let i = QCheck.fun1 R.observable H.arbitrary in
    let h = QCheck.fun1 H.observable I.arbitrary in
    let arbitrary = QCheck.(pair (b l r) (quad f g h i)) in
    QCheck.Test.make ~count:100
      ~name:"bimap (f % g) (h % i) = (bimap f h) % (bimap g i)" arbitrary
      (fun (x, (f_i, g_i, h_i, i_i)) ->
        let open Preface_core.Fun in
        let f = QCheck.Fn.apply f_i in
        let g = QCheck.Fn.apply g_i in
        let h = QCheck.Fn.apply h_i in
        let i = QCheck.Fn.apply i_i in
        let left = (Bifunctor.bimap (f % g) (h % i)) x
        and right = Bifunctor.(bimap f h % bimap g i) x in
        left = right)
  ;;

  let fst_parametricity =
    let g = QCheck.fun1 L.observable F.arbitrary in
    let f = QCheck.fun1 F.observable G.arbitrary in
    let arbitrary = QCheck.triple (b l r) f g in
    QCheck.Test.make ~count:100 ~name:"fst (f % g) = (fst f) % (fst g)"
      arbitrary (fun (x, f_i, g_i) ->
        let open Preface_core.Fun in
        let f = QCheck.Fn.apply f_i in
        let g = QCheck.Fn.apply g_i in
        let left = Bifunctor.fst (f % g) x
        and right = Bifunctor.(fst f % fst g) x in
        left = right)
  ;;

  let snd_parametricity =
    let g = QCheck.fun1 R.observable F.arbitrary in
    let f = QCheck.fun1 F.observable G.arbitrary in
    let arbitrary = QCheck.triple (b l r) f g in
    QCheck.Test.make ~count:100 ~name:"snd (f % g) = (snd f) % (snd g)"
      arbitrary (fun (x, f_i, g_i) ->
        let open Preface_core.Fun in
        let f = QCheck.Fn.apply f_i in
        let g = QCheck.Fn.apply g_i in
        let left = Bifunctor.snd (f % g) x
        and right = Bifunctor.(snd f % snd g) x in
        left = right)
  ;;

  let bimap_fst_id =
    let f = QCheck.fun1 L.observable F.arbitrary in
    let arbitrary = QCheck.pair (b l r) f in
    QCheck.Test.make ~count:100 ~name:"fst f = bimap f id" arbitrary
      (fun (x, f_i) ->
        let open Preface_core.Fun in
        let f = QCheck.Fn.apply f_i in
        let left = Bifunctor.bimap f id x
        and right = Bifunctor.fst f x in
        left = right)
  ;;

  let bimap_snd_id =
    let f = QCheck.fun1 R.observable F.arbitrary in
    let arbitrary = QCheck.pair (b l r) f in
    QCheck.Test.make ~count:100 ~name:"snd f = bimap id f" arbitrary
      (fun (x, f_i) ->
        let open Preface_core.Fun in
        let f = QCheck.Fn.apply f_i in
        let left = Bifunctor.bimap id f x
        and right = Bifunctor.snd f x in
        left = right)
  ;;

  let replace_fst =
    let inputs = QCheck.pair (b l r) G.arbitrary in
    QCheck.Test.make ~count:100 ~name:"replace = map % const" inputs
      (fun (x, value) ->
        let open Preface_core.Fun in
        let left = Bifunctor.replace_fst value x
        and right = Bifunctor.fst (const value) x in
        left = right)
  ;;

  let replace_snd =
    let inputs = QCheck.pair (b l r) G.arbitrary in
    QCheck.Test.make ~count:100 ~name:"replace = map % const" inputs
      (fun (x, value) ->
        let open Preface_core.Fun in
        let left = Bifunctor.replace_snd value x
        and right = Bifunctor.snd (const value) x in
        left = right)
  ;;

  let cases =
    ( Req.suite_name ^ " Bifunctor"
    , List.map QCheck_alcotest.to_alcotest
        [
          identity
        ; first
        ; second
        ; bimap_fst_snd
        ; bimap_parametricity
        ; fst_parametricity
        ; snd_parametricity
        ; bimap_fst_id
        ; bimap_snd_id
        ; replace_fst
        ; replace_snd
        ] )
  ;;
end
