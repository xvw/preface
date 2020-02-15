module Functor (F : sig
  type 'a t

  val pure : 'a -> 'a t

  val eq : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  include Preface_specs.FUNCTOR with type 'a t := 'a t
end) : sig
  val cases : unit Alcotest.test_case list
end = struct
  let subject a = Alcotest.(testable (F.pp (pp a)) (F.eq ( = )))

  open F

  let should_replace () =
    let expected = pure 42
    and computed = replace 42 (pure 41) in
    Alcotest.(check (subject int)) "should_replace" expected computed
  ;;

  let should_replace_with_infix_operator () =
    let expected = pure 42
    and computed = 42 <$ pure 41 in
    Alcotest.(check (subject int))
      "should_replace_with_infix_operator" expected computed
  ;;

  let should_flipped_replace_with_infix_operator () =
    let expected = pure 42
    and computed = pure 41 $> 42 in
    Alcotest.(check (subject int))
      "should_flipped_replace_with_infix_operator" expected computed
  ;;

  let should_map () =
    let expected = pure 42
    and computed = map (( + ) 1) (pure 41) in
    Alcotest.(check (subject int)) "should_map" expected computed
  ;;

  let should_void () =
    let expected = pure 42
    and computed =
      map (fun _ -> 42) (void (pure 41))
      (* void testable does not exist (yet!) *)
    in
    Alcotest.(check (subject int)) "should_void" expected computed
  ;;

  let should_map_with_infix_operator () =
    let expected = pure 42
    and computed = ( + ) 1 <$> pure 41 in
    Alcotest.(check (subject int))
      "should_map_with_infix_operator" expected computed
  ;;

  let should_flipped_map_with_infix_operator () =
    let expected = pure 42
    and computed = pure 41 <&> ( + ) 1 in
    Alcotest.(check (subject int))
      "should_flipped_map_with_infix_operator" expected computed
  ;;

  let cases =
    let open Alcotest in
    [
      test_case "Replace" `Quick should_replace
    ; test_case "Infix replace" `Quick should_replace_with_infix_operator
    ; test_case "Infix flipped replace" `Quick
        should_flipped_replace_with_infix_operator
    ; test_case "Void" `Quick should_void
    ; test_case "Map" `Quick should_map
    ; test_case "Infix map" `Quick should_map_with_infix_operator
    ; test_case "Infix flipped map" `Quick
        should_flipped_map_with_infix_operator
    ]
  ;;
end

module Applicative (A : sig
  type 'a t

  val eq : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  include Preface_specs.APPLICATIVE with type 'a t := 'a t
end) : sig
  val cases : unit Alcotest.test_case list
end = struct
  open A

  let subject a = Alcotest.(testable (A.pp (pp a)) (A.eq ( = )))

  let should_map () =
    let expected = pure 42
    and computed = map (( + ) 2) @@ pure 40 in
    Alcotest.(check (subject int)) "should_map" expected computed
  ;;

  let should_product_first () =
    let expected = pure 42
    and computed = fst <$> product (pure 42) (pure 2) in
    Alcotest.(check (subject int)) "should_product_first" expected computed
  ;;

  let should_product_second () =
    let expected = pure 42
    and computed = snd <$> product (pure 2) (pure 42) in
    Alcotest.(check (subject int)) "should_product_second" expected computed
  ;;

  let should_apply () =
    let expected = pure 42
    and computed = apply (pure @@ ( + ) 2) @@ pure 40 in
    Alcotest.(check (subject int)) "should_apply" expected computed
  ;;

  let should_lift () =
    let expected = pure 42
    and computed = lift (( + ) 2) (pure 40) in
    Alcotest.(check (subject int)) "should_lift" expected computed
  ;;

  let should_lift2 () =
    let expected = pure 42
    and computed = lift2 ( + ) (pure 40) (pure 2) in
    Alcotest.(check (subject int)) "should_lift2" expected computed
  ;;

  let should_lift3 () =
    let add a b c = a + b + c in
    let expected = pure 42
    and computed = lift3 add (pure 36) (pure 4) (pure 2) in
    Alcotest.(check (subject int)) "should_lift3" expected computed
  ;;

  let should_map_with_syntax () =
    let expected = pure 42
    and computed =
      let+ x = pure 40 in
      x + 2
    in
    Alcotest.(check (subject int)) "should_map" expected computed
  ;;

  let should_map_and_product_with_syntax () =
    let expected = pure 42
    and computed =
      let+ f = pure ( + )
      and+ x = pure 40
      and+ y = pure 2 in
      f x y
    in
    Alcotest.(check (subject int)) "should_map" expected computed
  ;;

  let should_apply_with_infix_operator () =
    let expected = pure 42
    and computed = ( + ) <$> pure 40 <*> pure 2 in
    Alcotest.(check (subject int))
      "should_apply_with_infix_operator" expected computed
  ;;

  let should_flipped_apply_with_infix_operator () =
    let expected = pure 42
    and computed = pure 40 <**> pure (( + ) 2) in
    Alcotest.(check (subject int))
      "should_flipped_apply_with_infix_operator" expected computed
  ;;

  let should_discard_with_infix_operator () =
    let expected = pure 42
    and computed = pure 2 *> pure 42 in
    Alcotest.(check (subject int))
      "should_discard_with_infix_operator" expected computed
  ;;

  let should_flipped_discard_with_infix_operator () =
    let expected = pure 42
    and computed = pure 42 <* pure 2 in
    Alcotest.(check (subject int))
      "should_flipped_apply_with_infix_operator" expected computed
  ;;

  let cases =
    let open Alcotest in
    [
      test_case "Map" `Quick should_map
    ; test_case "First of product" `Quick should_product_first
    ; test_case "Second of product" `Quick should_product_second
    ; test_case "Apply" `Quick should_apply
    ; test_case "Lift" `Quick should_lift
    ; test_case "Lift2" `Quick should_lift2
    ; test_case "Lift3" `Quick should_lift3
    ; test_case "Map using syntax" `Quick should_map_with_syntax
    ; test_case "Map and Product using syntax" `Quick
        should_map_and_product_with_syntax
    ; test_case "Infix Apply" `Quick should_apply_with_infix_operator
    ; test_case "Infix Flipped Apply" `Quick
        should_flipped_apply_with_infix_operator
    ; test_case "Infix Discard" `Quick should_discard_with_infix_operator
    ; test_case "Infix Flipped Discard" `Quick
        should_flipped_discard_with_infix_operator
    ]
  ;;
end

module Monad (M : sig
  type 'a t

  val eq : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  include Preface_specs.MONAD with type 'a t := 'a t
end) : sig
  val cases : unit Alcotest.test_case list
end = struct
  open M

  let subject a = Alcotest.(testable (M.pp (pp a)) (M.eq ( = )))

  let should_bind () =
    let expected = return 42
    and computed = bind (fun x -> return (x + 2)) @@ return 40 in
    Alcotest.(check (subject int)) "should_bind" expected computed
  ;;

  let should_map () =
    let expected = return 42
    and computed = map (( + ) 2) @@ return 40 in
    Alcotest.(check (subject int)) "should_map" expected computed
  ;;

  let should_join () =
    let expected = return 42
    and computed = join @@ return (return 42) in
    Alcotest.(check (subject int)) "should_join" expected computed
  ;;

  let should_compose_left_to_right () =
    let expected = return "42"
    and computed =
      compose_left_to_right
        (fun x -> return (x + 2))
        (fun x -> return (string_of_int x))
        40
    in
    Alcotest.(check (subject string))
      "should_compose_left_to_right" expected computed
  ;;

  let should_void () =
    let expected = return ()
    and computed = void @@ return 42 in
    Alcotest.(check (subject unit)) "should_void" expected computed
  ;;

  let should_compose_right_to_left () =
    let expected = return "42"
    and computed =
      compose_right_to_left
        (fun x -> return (string_of_int x))
        (fun x -> return (x + 2))
        40
    in
    Alcotest.(check (subject string))
      "should_compose_right_to_left" expected computed
  ;;

  let should_lift () =
    let expected = return 42
    and computed = lift (( + ) 2) @@ return 40 in
    Alcotest.(check (subject int)) "should_lift" expected computed
  ;;

  let should_lift2 () =
    let expected = return 42
    and computed = lift2 ( + ) (return 40) (return 2) in
    Alcotest.(check (subject int)) "should_lift2" expected computed
  ;;

  let should_lift3 () =
    let add a b c = a + b + c in
    let expected = return 42
    and computed = lift3 add (return 36) (return 4) (return 2) in
    Alcotest.(check (subject int)) "should_lift3" expected computed
  ;;

  let should_flipped_bind_with_syntax () =
    let expected = return 42
    and computed =
      let* x = return 2 in
      return (x + 40)
    in
    Alcotest.(check (subject int))
      "should_flipped_bind_with_syntax" expected computed
  ;;

  let should_map_with_infix_operator () =
    let expected = return 42
    and computed = ( + ) 2 =|< return 40 in
    Alcotest.(check (subject int))
      "should_map_with_infix_operator" expected computed
  ;;

  let should_flipped_map_with_infix_operator () =
    let expected = return 42
    and computed = return 40 >|= ( + ) 2 in
    Alcotest.(check (subject int))
      "should_flipped_map_with_infix_operator" expected computed
  ;;

  let should_flipped_bind_with_infix_operator () =
    let expected = return 42
    and computed = return 40 >>= (fun x -> return (x + 2)) in
    Alcotest.(check (subject int))
      "should_flipped_bind_with_infix_operator" expected computed
  ;;

  let should_bind_with_infix_operator () =
    let expected = return 42
    and computed = (fun x -> return (x + 2)) =<< return 40 in
    Alcotest.(check (subject int))
      "should_bind_with_infix_operator" expected computed
  ;;

  let should_compose_left_to_right_with_infix_operator () =
    let expected = return "42"
    and computed =
      ((fun x -> return (x + 2)) >=> (fun x -> return (string_of_int x))) 40
    in
    Alcotest.(check (subject string))
      "should_compose_left_to_right_with_infix_operator" expected computed
  ;;

  let should_compose_right_to_left_with_infix_operator () =
    let expected = return "42"
    and computed =
      ((fun x -> return (string_of_int x)) <=< (fun x -> return (x + 2))) 40
    in
    Alcotest.(check (subject string))
      "should_compose_left_to_right_with_infix_operator" expected computed
  ;;

  let should_discard_first_value () =
    let expected = return 42
    and computed = return "42" >> return 42 in
    Alcotest.(check (subject int))
      "should_discard_first_value" expected computed
  ;;

  let should_discard_second_value () =
    let expected = return 42
    and computed = return 42 << return "42" in
    Alcotest.(check (subject int))
      "should_discard_second_value" expected computed
  ;;

  let cases =
    let open Alcotest in
    [
      test_case "Bind" `Quick should_bind
    ; test_case "Map" `Quick should_map
    ; test_case "Join" `Quick should_join
    ; test_case "Compose left to right" `Quick should_compose_left_to_right
    ; test_case "Void" `Quick should_void
    ; test_case "Compose right to left" `Quick should_compose_right_to_left
    ; test_case "Infix Bind" `Quick should_bind_with_infix_operator
    ; test_case "Lift" `Quick should_lift
    ; test_case "Lift2" `Quick should_lift2
    ; test_case "Lift3" `Quick should_lift3
    ; test_case "Flipped Bind with syntax" `Quick
        should_flipped_bind_with_syntax
    ; test_case "Map with infix operator" `Quick should_map_with_infix_operator
    ; test_case "Flipped Map with infix operator" `Quick
        should_flipped_map_with_infix_operator
    ; test_case "Flipped Bind with infix operator" `Quick
        should_flipped_bind_with_infix_operator
    ; test_case "Bind with infix operator" `Quick
        should_bind_with_infix_operator
    ; test_case "Compose Left to Right with infix operator" `Quick
        should_compose_left_to_right_with_infix_operator
    ; test_case "Compose Right to Left with infix operator" `Quick
        should_compose_right_to_left_with_infix_operator
    ; test_case "Discard First value" `Quick should_discard_first_value
    ; test_case "Discard Second value" `Quick should_discard_second_value
    ]
  ;;
end
