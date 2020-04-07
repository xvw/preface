module type MONAD = Preface_specs.MONAD

module Make_with_post_hook
    (Monad : MONAD)
    (Req : Qcheck_helpers.REQ with type 'a t = 'a Monad.t)
    (Hook : Qcheck_helpers.HOOK with type 'a t = 'a Monad.t)
    (X : Qcheck_helpers.GENERATOR)
    (F : Qcheck_helpers.GENERATOR)
    (G : Qcheck_helpers.GENERATOR)
    (H : Qcheck_helpers.GENERATOR) : Qcheck_helpers.ALCOTEST_SUITE = struct
  let m = Req.arbitrary

  let a = X.arbitrary

  let join_map_1 =
    QCheck.Test.make ~count:100 ~name:"join % join = join % map join"
      (m (m (m a)))
      (fun x ->
        let open Monad in
        let open Preface_core.Fun in
        let left = (join % join) x
        and right = (join % map join) x in
        Hook.(apply left = apply right))
  ;;

  let join_map_2 =
    QCheck.Test.make ~count:100 ~name:"join % map return = join % return = id"
      (m a) (fun x ->
        let open Monad in
        let open Preface_core.Fun in
        let left = (join % map return) x
        and center = (join % return) x
        and right = id x in
        Hook.(apply left = apply center && apply center = apply right))
  ;;

  let natural_transformation_1 =
    QCheck.Test.make ~count:100 ~name:"map id = id" (m a) (fun x ->
        let open Monad in
        let open Preface_core.Fun in
        let left = map id x
        and right = id x in
        Hook.(apply left = apply right))
  ;;

  let natural_transformation_2 =
    let f = QCheck.fun1 G.observable F.arbitrary in
    let g = QCheck.fun1 X.observable G.arbitrary in
    let inputs = QCheck.triple (m a) f g in
    QCheck.Test.make ~count:100 ~name:"map (f % g) = map f % map g" inputs
      (fun (x, f_i, g_i) ->
        let open Preface_core.Fun in
        let f = QCheck.Fn.apply f_i
        and g = QCheck.Fn.apply g_i in
        let left = Monad.map (f % g) x
        and right = (Monad.map f % Monad.map g) x in
        Hook.(apply left = apply right))
  ;;

  let natural_transformation_3 =
    let f = QCheck.fun1 X.observable F.arbitrary in
    let inputs = QCheck.pair (m (m a)) f in
    QCheck.Test.make ~count:100 ~name:"map f % join = join % map (map f)" inputs
      (fun (x, f_i) ->
        let open Monad in
        let open Preface_core.Fun in
        let f = QCheck.Fn.apply f_i in
        let left = (map f % join) x
        and right = (join % map (map f)) x in
        Hook.(apply left = apply right))
  ;;

  let natural_transformation_4 =
    let f = QCheck.fun1 X.observable F.arbitrary in
    let inputs = QCheck.pair X.arbitrary f in
    QCheck.Test.make ~count:100 ~name:"map f % return = return f " inputs
      (fun (x, f_i) ->
        let open Monad in
        let open Preface_core.Fun in
        let f = QCheck.Fn.apply f_i in
        let left = (map f % return) x
        and right = (return % f) x in
        Hook.(apply left = apply right))
  ;;

  let left_identity =
    let f = QCheck.fun1 X.observable (m F.arbitrary) in
    let inputs = QCheck.pair X.arbitrary f in
    QCheck.Test.make ~count:100 ~name:"return x >>= f = f x" inputs
      (fun (x, f_i) ->
        let open Monad in
        let f = QCheck.Fn.apply f_i in
        let left = return x >>= f
        and right = f x in
        Hook.(apply left = apply right))
  ;;

  let right_identity =
    QCheck.Test.make ~count:100 ~name:"x >>= return = x" (m a) (fun x ->
        let open Monad in
        let left = x >>= return
        and right = x in
        Hook.(apply left = apply right))
  ;;

  let associativity =
    let f = QCheck.fun1 X.observable (m F.arbitrary) in
    let g = QCheck.fun1 F.observable (m G.arbitrary) in
    let inputs = QCheck.triple (m a) f g in
    QCheck.Test.make ~count:100
      ~name:"(x >>= f) >>= g = x >>= (fun y -> f y >>= g)" inputs
      (fun (x, f_i, g_i) ->
        let open Monad in
        let f = QCheck.Fn.apply f_i in
        let g = QCheck.Fn.apply g_i in
        let left = x >>= f >>= g
        and right = x >>= (fun y -> f y >>= g) in
        Hook.(apply left = apply right))
  ;;

  let kleisli_left_identity =
    let f = QCheck.fun1 X.observable (m F.arbitrary) in
    let inputs = QCheck.pair X.arbitrary f in
    QCheck.Test.make ~count:100 ~name:"return >=> f = f" inputs (fun (x, f_i) ->
        let open Monad in
        let f = QCheck.Fn.apply f_i in
        let left = (return >=> f) x
        and right = f x in
        Hook.(apply left = apply right))
  ;;

  let kleisli_right_identity =
    let f = QCheck.fun1 X.observable (m F.arbitrary) in
    let inputs = QCheck.pair X.arbitrary f in
    QCheck.Test.make ~count:100 ~name:"f >=> return = f" inputs (fun (x, f_i) ->
        let open Monad in
        let f = QCheck.Fn.apply f_i in
        let left = (f >=> return) x
        and right = f x in
        Hook.(apply left = apply right))
  ;;

  let kleisli_associativity =
    let f = QCheck.fun1 X.observable (m F.arbitrary) in
    let g = QCheck.fun1 F.observable (m G.arbitrary) in
    let h = QCheck.fun1 G.observable (m H.arbitrary) in
    let inputs = QCheck.quad a f g h in
    QCheck.Test.make ~count:100 ~name:"(f >=> g) >=> h = f >=> (g >=> h)" inputs
      (fun (x, f_i, g_i, h_i) ->
        let open Monad in
        let f = QCheck.Fn.apply f_i in
        let g = QCheck.Fn.apply g_i in
        let h = QCheck.Fn.apply h_i in
        let left = (f >=> g >=> h) x
        and right = (f >=> (g >=> h)) x in
        Hook.(apply left = apply right))
  ;;

  let cases =
    ( Req.suite_name ^ " monad"
    , List.map QCheck_alcotest.to_alcotest
        [
          join_map_1
        ; join_map_2
        ; natural_transformation_1
        ; natural_transformation_2
        ; natural_transformation_3
        ; natural_transformation_4
        ; left_identity
        ; right_identity
        ; associativity
        ; kleisli_left_identity
        ; kleisli_right_identity
        ; kleisli_associativity
        ] )
  ;;
end

module Make
    (Monad : MONAD)
    (Req : Qcheck_helpers.REQ with type 'a t = 'a Monad.t) =
  Make_with_post_hook (Monad) (Req)
    (struct
      type 'a t = 'a Monad.t

      let apply x = Obj.magic x
    end)
