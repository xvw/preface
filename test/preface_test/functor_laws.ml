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

  let cases =
    ( Req.suite_name ^ " functor"
    , List.map QCheck_alcotest.to_alcotest
        [ preserve_identity; preserve_morphisms ] )
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
