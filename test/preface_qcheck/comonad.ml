module Make_hooked
    (C : Preface_specs.COMONAD)
    (R : Requirement.INPUT_T1 with type 'a t = 'a C.t)
    (Hook : Requirement.HOOK with type 'a t = 'a C.t)
    (Obs : sig
      type 'a t

      val f : 'a QCheck.Observable.t -> 'a t QCheck.Observable.t
    end
    with type 'a t = 'a C.t)
    (P : Sample.PACK) : Requirement.OUTPUT = struct
  open QCheck

  open Helper.Make_for_t1 (R) (P)

  module Underlying = Preface_make.Comonad.Via_extend (C)

  (* Test Comonad laws *)

  let preserve_identity =
    let test_name = "extend extract = id"
    and test_arbitrary = over t1
    and test x =
      let open C in
      let open Preface_stdlib.Fun in
      let left = extend extract x
      and right = id x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let extract_extend =
    let test_name = "extract % extend f = f"
    and test_arbitrary = pair (fun1 (Obs.f t1') t2) (over t1)
    and test (f', x) =
      let open C in
      let open Preface_stdlib.Fun in
      let f = Fn.apply f' in
      let left = (extract % extend f) x
      and right = f x in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let extend_extend =
    let test_name = "extend f % extend g = extend (f % extend g)"
    and test_arbitrary =
      triple (fun1 (Obs.f t3') t4) (fun1 (Obs.f t1') t3) (over t1)
    and test (f', g', x) =
      let open C in
      let open Preface_stdlib.Fun in
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let left = (extend f % extend g) x
      and right = (extend (f % extend g)) x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let infix_extract_extend =
    let test_name = "f =>= extract = f"
    and test_arbitrary = pair (fun1 (Obs.f t1') t2) (over t1)
    and test (f', x) =
      let open C in
      let f = Fn.apply f' in
      let left = (f =>= extract) x
      and right = f x in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let rev_infix_extract_extend =
    let test_name = "extract =>= f = f"
    and test_arbitrary = pair (fun1 (Obs.f t1') t2) (over t1)
    and test (f', x) =
      let open C in
      let f = Fn.apply f' in
      let left = (extract =>= f) x
      and right = f x in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let infix_extend_triple =
    let test_name = "(f =>= g) =>= h = f =>= (g =>= h)"
    and test_arbitrary =
      quad
        (fun1 (Obs.f t1') t2)
        (fun1 (Obs.f t2') t3)
        (fun1 (Obs.f t3') t4)
        (over t1)
    and test (f', g', h', x) =
      let open C in
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let h = Fn.apply h' in
      let left = (f =>= g =>= h) x
      and right = (f =>= (g =>= h)) x in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let duplicate_preserve_identity =
    let test_name = "extract % duplicate = id"
    and test_arbitrary = over t1
    and test x =
      let open C in
      let open Preface_stdlib.Fun in
      let left = (extract % duplicate) x
      and right = id x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let map_duplicate_preserve_identity =
    let test_name = "map extract % duplicate = id"
    and test_arbitrary = over t1
    and test x =
      let open C in
      let open Preface_stdlib.Fun in
      let left = (map extract % duplicate) x
      and right = id x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let map_duplicate_duplicate =
    let test_name = "duplicate % duplicate = map duplicate % duplicate"
    and test_arbitrary = over t1
    and test x =
      let open C in
      let open Preface_stdlib.Fun in
      let left =
        (duplicate % duplicate) x
        |> C.map (fun x -> Hook.apply (C.map Hook.apply x))
      and right =
        (map duplicate % duplicate) x
        |> C.map (fun x -> Hook.apply (C.map Hook.apply x))
      in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let extend_map_duplicate =
    let test_name = "extend f = map f % duplicate"
    and test_arbitrary = pair (fun1 (Obs.f t1') t2) (over t1)
    and test (f', x) =
      let open C in
      let open Preface_stdlib.Fun in
      let f = Fn.apply f' in
      let left = (extend f) x
      and right = (map f % duplicate) x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let duplicate_extract_id =
    let test_name = "duplicate = extend id"
    and test_arbitrary = over t1
    and test x =
      let open C in
      let left = duplicate x |> C.map Hook.apply
      and right = (extend Fun.id) x |> C.map Hook.apply in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let map_extend_extract =
    let test_name = "map f = extend (f % extract)"
    and test_arbitrary = pair (fun1 t1' t2) (over t1)
    and test (f', x) =
      let open C in
      let open Preface_stdlib.Fun in
      let f = Fn.apply f' in
      let left = map f x
      and right = (extend (f % extract)) x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  (* Test underlying Comonad *)

  let extract =
    let test_name = "extract"
    and test_arbitrary = over t1
    and test x =
      let left = C.extract x
      and right = Underlying.extract x in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let duplicate =
    let test_name = "duplicate"
    and test_arbitrary = over t1
    and test x =
      let left = C.duplicate x |> C.map Hook.apply
      and right = Underlying.duplicate x |> Underlying.map Hook.apply in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let map =
    let test_name = "map"
    and test_arbitrary = pair (fun1 t1' t2) (over t1)
    and test (f', x) =
      let f = Fn.apply f' in
      let left = C.map f x
      and right = Underlying.map f x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let infix_apply =
    let test_name = "<@>"
    and test_arbitrary = pair (over (fun1 t1' t2)) (over t1)
    and test (f', x) =
      let f = C.map Fn.apply f' in
      let left = C.(f <@> x)
      and right = Underlying.(f <@> x) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let rinfix_apply =
    let test_name = "<@@>"
    and test_arbitrary = pair (over (fun1 t1' t2)) (over t1)
    and test (f', x) =
      let f = C.map Fn.apply f' in
      let left = C.(x <@@> f)
      and right = Underlying.(x <@@> f) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let infix_discard =
    let test_name = "@>"
    and test_arbitrary = pair (over t1) (over Arbitrary.unit)
    and test (x, void) =
      let left = C.(void @> x)
      and right = Underlying.(void @> x) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let rinfix_discard =
    let test_name = "<@"
    and test_arbitrary = pair (over t1) (over Arbitrary.unit)
    and test (x, void) =
      let left = C.(x <@ void)
      and right = Underlying.(x <@ void) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let extend =
    let test_name = "extend"
    and test_arbitrary = pair (fun1 (Obs.f t1') t2) (over t1)
    and test (f', x) =
      let f = Fn.apply f' in
      let left = C.extend f x
      and right = Underlying.extend f x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let infix_extend =
    let test_name = "<<="
    and test_arbitrary = pair (fun1 (Obs.f t1') t2) (over t1)
    and test (f', x) =
      let f = Fn.apply f' in
      let left = C.(f <<= x)
      and right = Underlying.(f <<= x) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let rinfix_extend =
    let test_name = "=>>"
    and test_arbitrary = pair (fun1 (Obs.f t1') t2) (over t1)
    and test (f', x) =
      let f = Fn.apply f' in
      let left = C.(x =>> f)
      and right = Underlying.(x =>> f) in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let let_syntax_extend =
    let test_name = "let@"
    and test_arbitrary = pair (fun1 (Obs.f t1') t2) (over t1)
    and test (f', x) =
      let f = Fn.apply f' in
      let left =
        let open C.Syntax in
        let@ a = x in
        f a
      and right =
        let open Underlying.Syntax in
        let@ a = x in
        f a
      in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let compose_left_to_right =
    let test_name = "compose_left_to_right"
    and test_arbitrary =
      triple (fun1 (Obs.f t1') t2) (fun1 (Obs.f t2') t3) (over t1)
    and test (f', g', x) =
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let left = C.compose_left_to_right f g x
      and right = Underlying.compose_left_to_right f g x in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let compose_right_to_left =
    let test_name = "compose_right_to_left"
    and test_arbitrary =
      triple (fun1 (Obs.f t2') t3) (fun1 (Obs.f t1') t2) (over t1)
    and test (f', g', x) =
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let left = C.compose_right_to_left f g x
      and right = Underlying.compose_right_to_left f g x in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let infix_compose_left_to_right =
    let test_name = "=>="
    and test_arbitrary =
      triple (fun1 (Obs.f t1') t2) (fun1 (Obs.f t2') t3) (over t1)
    and test (f', g', x) =
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let left = C.(f =>= g) x
      and right = Underlying.(f =>= g) x in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let infix_compose_right_to_left =
    let test_name = "=<="
    and test_arbitrary =
      triple (fun1 (Obs.f t2') t3) (fun1 (Obs.f t1') t2) (over t1)
    and test (f', g', x) =
      let f = Fn.apply f' in
      let g = Fn.apply g' in
      let left = C.(f =<= g) x
      and right = Underlying.(f =<= g) x in
      left = right
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let lift =
    let test_name = "lift"
    and test_arbitrary = pair (fun1 t1' t2) (over t1)
    and test (f', x) =
      let f = Fn.apply f' in
      let left = C.lift f x
      and right = Underlying.lift f x in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let lift2 =
    let test_name = "lift2"
    and test_arbitrary = triple (fun2 t1' t2' t3) (over t1) (over t2)
    and test (f', x, y) =
      let f = Fn.apply f' in
      let left = C.lift2 f x y
      and right = Underlying.lift2 f x y in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let lift3 =
    let test_name = "lift3"
    and test_arbitrary =
      quad (fun3 t1' t2' t3' t4) (over t1) (over t2) (over t3)
    and test (f', x, y, z) =
      let f = Fn.apply f' in
      let left = C.lift3 f x y z
      and right = Underlying.lift3 f x y z in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let let_syntax_map =
    let test_name = "let+"
    and test_arbitrary = pair (fun1 t1' t2) (over t1)
    and test (f', x) =
      let f = Fn.apply f' in
      let left =
        let open C.Syntax in
        let+ a = x in
        f a
      and right =
        let open Underlying.Syntax in
        let+ a = x in
        f a
      in
      Hook.(apply left = apply right)
    in
    Test.make ~name:test_name ~count:R.size test_arbitrary test
  ;;

  let cases =
    [
      ( "Comonad " ^ R.name ^ " laws"
      , [
          preserve_identity
        ; extract_extend
        ; extend_extend
        ; infix_extract_extend
        ; rev_infix_extract_extend
        ; infix_extend_triple
        ; duplicate_preserve_identity
        ; map_duplicate_preserve_identity
        ; map_duplicate_duplicate
        ; extend_map_duplicate
        ; duplicate_extract_id
        ; map_extend_extract
        ]
        |> List.map QCheck_alcotest.to_alcotest )
    ; ( "Comonad " ^ R.name ^ " has expected behaviour"
      , [
          extract
        ; duplicate
        ; map
        ; extend
        ; compose_left_to_right
        ; compose_right_to_left
        ; lift
        ; lift2
        ; lift3
        ; let_syntax_extend
        ; let_syntax_map
        ; infix_extend
        ; rinfix_extend
        ; infix_compose_right_to_left
        ; infix_compose_left_to_right
        ; infix_apply
        ; rinfix_apply
        ; infix_discard
        ; rinfix_discard
        ]
        |> List.map QCheck_alcotest.to_alcotest )
    ]
  ;;
end
