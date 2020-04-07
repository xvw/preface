module type APPLICATIVE = Preface_specs.APPLICATIVE

module Make_with_post_hook
    (Applicative : APPLICATIVE)
    (Req : Qcheck_helpers.REQ with type 'a t = 'a Applicative.t)
    (Hook : Qcheck_helpers.HOOK with type 'a t = 'a Applicative.t)
    (X : Qcheck_helpers.GENERATOR)
    (F : Qcheck_helpers.GENERATOR)
    (U : Qcheck_helpers.GENERATOR)
    (V : Qcheck_helpers.GENERATOR)
    (W : Qcheck_helpers.GENERATOR) : Qcheck_helpers.ALCOTEST_SUITE = struct
  let preserve_identity =
    QCheck.Test.make ~count:100 ~name:"pure id <*> x = x"
      (Req.arbitrary X.arbitrary) (fun x ->
        let open Applicative in
        let left = Infix.(pure (fun i -> i) <*> x)
        and right = x in
        Hook.(apply left = apply right))
  ;;

  let homomorphism =
    let f_x = QCheck.pair X.arbitrary (QCheck.fun1 X.observable F.arbitrary) in
    QCheck.Test.make ~count:100 ~name:"pure f <*> pure x = pure (f x)" f_x
      (fun (x, fi) ->
        let open Applicative in
        let f = QCheck.Fn.apply fi in
        let left = Infix.(pure f <*> pure x)
        and right = pure (f x) in
        Hook.(apply left = apply right))
  ;;

  let interchange =
    let u_f = QCheck.fun1 X.observable U.arbitrary |> Req.arbitrary in
    let u_app = QCheck.pair X.arbitrary u_f in
    QCheck.Test.make ~count:100 ~name:"u <*> pure x = pure ((|>) x) <*> u" u_app
      (fun (y, ui) ->
        let open Applicative in
        let open Infix in
        let u = QCheck.Fn.apply <$> ui in
        let left = u <*> pure y
        and right = pure (( |> ) y) <*> u in
        Hook.(apply left = apply right))
  ;;

  let composition =
    let arbitrary =
      QCheck.triple
        (QCheck.fun1 V.observable X.arbitrary |> Req.arbitrary)
        (QCheck.fun1 W.observable V.arbitrary |> Req.arbitrary)
        (Req.arbitrary W.arbitrary)
    in
    QCheck.Test.make ~count:100
      ~name:"pure ( % ) <*> u <*> v <*> w = u <*> (v <*> w)" arbitrary
      (fun (ui, vi, w) ->
        let open Applicative in
        let open Infix in
        let u = QCheck.Fn.apply <$> ui in
        let v = QCheck.Fn.apply <$> vi in
        let left = pure (fun f g x -> f (g x)) <*> u <*> v <*> w
        and right = u <*> (v <*> w) in
        Hook.(apply left = apply right))
  ;;

  let additional_laws_1 =
    let arbitrary = Req.arbitrary X.arbitrary in
    QCheck.Test.make ~count:100 ~name:"u *> v = (id <$ u) <*> v" arbitrary
      (fun v ->
        let open Applicative.Infix in
        let left = Applicative.pure () *> v
        and right = (fun x -> x) <$ Applicative.pure () <*> v in
        Hook.(apply left = apply right))
  ;;

  let additional_laws_2 =
    let arbitrary = Req.arbitrary X.arbitrary in
    QCheck.Test.make ~count:100 ~name:"u <* v = liftA2 const u v" arbitrary
      (fun v ->
        let open Applicative.Infix in
        let open Preface_core.Fun in
        let left = v <* Applicative.pure ()
        and right = Applicative.lift2 constant v (Applicative.pure ()) in
        Hook.(apply left = apply right))
  ;;

  let cases =
    ( Req.suite_name ^ " applicative"
    , List.map QCheck_alcotest.to_alcotest
        [
          preserve_identity
        ; homomorphism
        ; interchange
        ; composition
        ; additional_laws_1
        ; additional_laws_2
        ] )
  ;;
end

module Make
    (Applicative : APPLICATIVE)
    (Req : Qcheck_helpers.REQ with type 'a t = 'a Applicative.t) =
  Make_with_post_hook (Applicative) (Req)
    (struct
      type 'a t = 'a Applicative.t

      let apply x = Obj.magic x
    end)
