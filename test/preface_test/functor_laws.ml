module type FUNCTOR = Preface_specs.FUNCTOR

module Make_with_post_hook
    (Functor : FUNCTOR)
    (Req : Qcheck_helpers.REQ with type 'a t = 'a Functor.t)
    (Hook : Qcheck_helpers.HOOK with type 'a t = 'a Functor.t)
    (X : Qcheck_helpers.GENERATOR)
    (F : Qcheck_helpers.GENERATOR)
    (G : Qcheck_helpers.GENERATOR) : Qcheck_helpers.ALCOTEST_SUITE = struct
  let preserve_identity =
    QCheck.Test.make ~count:100 ~name:"map id = id" (Req.arbitrary X.arbitrary)
      (fun x ->
        let left = Functor.map (fun i -> i) x
        and right = x in
        Hook.(apply left = apply right))
  ;;

  let preserve_morphisms =
    let f = QCheck.fun1 G.observable F.arbitrary in
    let g = QCheck.fun1 X.observable G.arbitrary in
    let inputs = QCheck.triple (Req.arbitrary X.arbitrary) f g in
    QCheck.Test.make ~count:100 ~name:"map (f % g) = map f % map g" inputs
      (fun (x, f_i, g_i) ->
        let f = QCheck.Fn.apply f_i
        and g = QCheck.Fn.apply g_i in
        let left = Functor.map (fun i -> f (g i)) x
        and right = (Functor.map f) (Functor.map g x) in
        Hook.(apply left = apply right))
  ;;

  let replace =
    let inputs = QCheck.pair (Req.arbitrary X.arbitrary) F.arbitrary in
    QCheck.Test.make ~count:100 ~name:"replace = map % const" inputs
      (fun (x, value) ->
        let open Preface_core.Fun in
        let left = Functor.replace value x
        and right = (Functor.map % const) value x in
        Hook.(apply left = apply right))
  ;;

  let void =
    let inputs = Req.arbitrary X.arbitrary in
    QCheck.Test.make ~count:100 ~name:"void = replace ()" inputs (fun x ->
        let left = Functor.void x
        and right = Functor.replace () x in
        Hook.(apply left = apply right))
  ;;

  let infix_map =
    let f = QCheck.fun1 X.observable F.arbitrary in
    let inputs = QCheck.pair f (Req.arbitrary X.arbitrary) in
    QCheck.Test.make ~count:100 ~name:"<$> = map" inputs (fun (f_i, x) ->
        let f = QCheck.Fn.apply f_i in
        let left = Functor.(f <$> x)
        and right = Functor.map f x in
        Hook.(apply left = apply right))
  ;;

  let infix_rmap =
    let f = QCheck.fun1 X.observable F.arbitrary in
    let inputs = QCheck.pair f (Req.arbitrary X.arbitrary) in
    QCheck.Test.make ~count:100 ~name:"<&> = flip % map" inputs (fun (f_i, x) ->
        let f = QCheck.Fn.apply f_i in
        let left = Functor.(x <&> f)
        and right = Functor.map f x in
        Hook.(apply left = apply right))
  ;;

  let infix_replace =
    let inputs = QCheck.pair (Req.arbitrary X.arbitrary) F.arbitrary in
    QCheck.Test.make ~count:100 ~name:"%> = replace" inputs (fun (x, value) ->
        let open Preface_core.Fun in
        let left = Functor.(value <$ x)
        and right = (Functor.map % const) value x in
        Hook.(apply left = apply right))
  ;;

  let infix_rreplace =
    let inputs = QCheck.pair (Req.arbitrary X.arbitrary) F.arbitrary in
    QCheck.Test.make ~count:100 ~name:"<% = flip % replace" inputs
      (fun (x, value) ->
        let open Preface_core.Fun in
        let left = Functor.(x $> value)
        and right = (Functor.map % const) value x in
        Hook.(apply left = apply right))
  ;;

  let cases =
    ( Req.suite_name ^ " functor"
    , List.map QCheck_alcotest.to_alcotest
        [
          preserve_identity
        ; preserve_morphisms
        ; replace
        ; void
        ; infix_map
        ; infix_rmap
        ; infix_replace
        ; infix_rreplace
        ] )
  ;;
end

module Make
    (Functor : FUNCTOR)
    (Req : Qcheck_helpers.REQ with type 'a t = 'a Functor.t) =
  Make_with_post_hook (Functor) (Req)
    (struct
      type 'a t = 'a Functor.t

      let apply x = Obj.magic x
    end)
