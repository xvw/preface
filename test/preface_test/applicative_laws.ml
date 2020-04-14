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

  let apply =
    let f_x =
      QCheck.pair
        (Req.arbitrary X.arbitrary)
        (QCheck.fun1 X.observable F.arbitrary)
    in
    QCheck.Test.make ~count:100
      ~name:"apply f a = map (fun (f, a) -> f a) @@ product f a" f_x
      (fun (x, fi) ->
        let open Applicative in
        let f = pure (QCheck.Fn.apply fi) in
        let left = apply f x
        and right = map (fun (f, a) -> f a) @@ product f x in
        Hook.(apply left = apply right))
  ;;

  let map =
    let f_x =
      QCheck.pair
        (Req.arbitrary X.arbitrary)
        (QCheck.fun1 X.observable F.arbitrary)
    in
    QCheck.Test.make ~count:100 ~name:"map f a = apply (pure f) a" f_x
      (fun (x, fi) ->
        let open Applicative in
        let f = QCheck.Fn.apply fi in
        let left = map f x
        and right = apply (pure f) x in
        Hook.(apply left = apply right))
  ;;

  let product =
    let f_x =
      QCheck.pair (Req.arbitrary X.arbitrary) (Req.arbitrary F.arbitrary)
    in
    QCheck.Test.make ~count:100
      ~name:"product a b = apply (apply (pure (fun a  -> (a, b)))) b" f_x
      (fun (a, b) ->
        let open Applicative in
        let left = product a b
        and right = apply (apply (pure (fun a b -> (a, b))) a) b in
        Hook.(apply left = apply right))
  ;;

  let lift =
    let f_x =
      QCheck.pair
        (Req.arbitrary X.arbitrary)
        (QCheck.fun1 X.observable F.arbitrary)
    in
    QCheck.Test.make ~count:100 ~name:"lift = map" f_x (fun (x, fi) ->
        let open Applicative in
        let f = QCheck.Fn.apply fi in
        let left = map f x
        and right = lift f x in
        Hook.(apply left = apply right))
  ;;

  let lift2 =
    let f_x =
      QCheck.triple
        (Req.arbitrary X.arbitrary)
        (Req.arbitrary F.arbitrary)
        (QCheck.fun2 X.observable F.observable F.arbitrary)
    in
    QCheck.Test.make ~count:100 ~name:"lift2 f a = apply @@ apply (pure f) a"
      f_x (fun (a, b, fi) ->
        let open Applicative in
        let f = QCheck.Fn.apply fi in
        let left = (apply @@ apply (pure f) a) b
        and right = lift2 f a b in
        Hook.(apply left = apply right))
  ;;

  let lift3 =
    let f_x =
      QCheck.quad
        (Req.arbitrary X.arbitrary)
        (Req.arbitrary F.arbitrary)
        (Req.arbitrary U.arbitrary)
        (QCheck.fun3 X.observable F.observable U.observable U.arbitrary)
    in
    QCheck.Test.make ~count:100
      ~name:"lift3 f a = apply @@ apply (apply (pure f) a) b" f_x
      (fun (a, b, c, fi) ->
        let open Applicative in
        let f = QCheck.Fn.apply fi in
        let left = (apply @@ apply (apply (pure f) a) b) c
        and right = lift3 f a b c in
        Hook.(apply left = apply right))
  ;;

  let replace =
    let inputs = QCheck.pair (Req.arbitrary X.arbitrary) F.arbitrary in
    QCheck.Test.make ~count:100 ~name:"replace = map % const" inputs
      (fun (x, value) ->
        let open Preface_core.Fun in
        let left = Applicative.replace value x
        and right = (Applicative.map % const) value x in
        Hook.(apply left = apply right))
  ;;

  let infix_map =
    let f = QCheck.fun1 X.observable F.arbitrary in
    let inputs = QCheck.pair f (Req.arbitrary X.arbitrary) in
    QCheck.Test.make ~count:100 ~name:"<$> = map" inputs (fun (f_i, x) ->
        let f = QCheck.Fn.apply f_i in
        let left = Applicative.(f <$> x)
        and right = Applicative.map f x in
        Hook.(apply left = apply right))
  ;;

  let infix_replace =
    let inputs = QCheck.pair (Req.arbitrary X.arbitrary) F.arbitrary in
    QCheck.Test.make ~count:100 ~name:"%> = replace" inputs (fun (x, value) ->
        let open Preface_core.Fun in
        let left = Applicative.(value <$ x)
        and right = (Applicative.map % const) value x in
        Hook.(apply left = apply right))
  ;;

  let infix_rreplace =
    let inputs = QCheck.pair (Req.arbitrary X.arbitrary) F.arbitrary in
    QCheck.Test.make ~count:100 ~name:"<% = flip % replace" inputs
      (fun (x, value) ->
        let open Preface_core.Fun in
        let left = Applicative.(x $> value)
        and right = (Applicative.map % const) value x in
        Hook.(apply left = apply right))
  ;;

  let infix_apply =
    let f_x =
      QCheck.pair
        (Req.arbitrary X.arbitrary)
        (QCheck.fun1 X.observable F.arbitrary)
    in
    QCheck.Test.make ~count:100 ~name:"<*> = apply" f_x (fun (x, fi) ->
        let open Applicative in
        let f = pure (QCheck.Fn.apply fi) in
        let left = apply f x
        and right = f <*> x in
        Hook.(apply left = apply right))
  ;;

  let infix_rapply =
    let f_x =
      QCheck.pair
        (Req.arbitrary X.arbitrary)
        (QCheck.fun1 X.observable F.arbitrary)
    in
    QCheck.Test.make ~count:100 ~name:"<**> = flip % apply" f_x (fun (x, fi) ->
        let open Applicative in
        let f = pure (QCheck.Fn.apply fi) in
        let left = apply f x
        and right = x <**> f in
        Hook.(apply left = apply right))
  ;;

  let syntax_map =
    let f_x =
      QCheck.pair
        (Req.arbitrary X.arbitrary)
        (QCheck.fun1 X.observable F.arbitrary)
    in
    QCheck.Test.make ~count:100 ~name:"(let+ x = a in f x) = (f <$> a)" f_x
      (fun (x, fi) ->
        let open Applicative in
        let f = QCheck.Fn.apply fi in
        let left = map f x
        and right =
          let+ a = x in
          f a
        in
        Hook.(apply left = apply right))
  ;;

  let syntax_product =
    let f_x =
      QCheck.pair (Req.arbitrary X.arbitrary) (Req.arbitrary F.arbitrary)
    in
    QCheck.Test.make ~count:100
      ~name:"(let+ x = a and+ y = b in (a, b)) = (product a b)" f_x
      (fun (a, b) ->
        let open Applicative in
        let left = product a b
        and right =
          let+ x = a
          and+ y = b in
          (x, y)
        in
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
        ; apply
        ; map
        ; product
        ; lift
        ; lift2
        ; lift3
        ; replace
        ; infix_map
        ; infix_replace
        ; infix_rreplace
        ; infix_apply
        ; infix_rapply
        ; syntax_map
        ; syntax_product
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
