module type LAWS = sig
  module Selective : Preface_specs.SELECTIVE
  include Applicative.LAWS with module Applicative := Selective

  val selective_identity :
    unit -> (('a, 'a) Either.t Selective.t, 'a Selective.t) Law.t

  val selective_distributivity :
       unit
    -> ( ('a, 'b) Either.t
       , ('a -> 'b) Selective.t -> ('a -> 'b) Selective.t -> 'b Selective.t )
       Law.t

  val selective_associativity :
       unit
    -> ( ('a, 'b) Either.t Selective.t
       ,    ('c, 'a -> 'b) Either.t Selective.t
         -> ('c -> 'a -> 'b) Selective.t
         -> 'b Selective.t )
       Law.t

  val selective_apply_pure_function_to_the_result :
       unit
    -> ( 'a -> 'b
       ,    ('c, 'a) Either.t Selective.t
         -> ('c -> 'a) Selective.t
         -> 'b Selective.t )
       Law.t

  val selective_apply_pure_function_to_left_case_of_the_first_argument :
       unit
    -> ( 'a -> 'b
       ,    ('a, 'c) Either.t Selective.t
         -> ('b -> 'c) Selective.t
         -> 'c Selective.t )
       Law.t

  val selective_apply_pure_function_to_the_second_argument :
       unit
    -> ( 'a -> 'b -> 'c
       , ('b, 'c) Either.t Selective.t -> 'a Selective.t -> 'c Selective.t )
       Law.t

  val selective_generalized_identity :
    unit -> (('a, 'b) Either.t Selective.t, ('a -> 'b) -> 'b Selective.t) Law.t
end

module type LAWS_RIGID = sig
  include LAWS

  val selective_rigid_apply :
    unit -> (('a -> 'b) Selective.t, 'a Selective.t -> 'b Selective.t) Law.t

  val selective_rigid_interchange :
       unit
    -> ( 'a Selective.t
       ,    ('b, 'c) Either.t Selective.t
         -> ('b -> 'c) Selective.t
         -> 'c Selective.t )
       Law.t
end

module For (S : Preface_specs.SELECTIVE) : LAWS with module Selective := S =
struct
  open Law
  open Preface_core.Fun
  include Applicative.For (S)

  let selective_identity () =
    let lhs x = S.(x <*? pure id)
    and rhs x = S.(Either.fold ~left:id ~right:id <$> x) in

    law "Identity"
      ~lhs:("x <*? pure Fun.id" =~ lhs)
      ~rhs:("Either.case Fun.id Fun.id <$> x" =~ rhs)
  ;;

  let selective_distributivity () =
    let lhs x y z = S.(pure x <*? Infix.(replace () y *> z))
    and rhs x y z =
      S.(replace () Infix.(pure x <*? y) *> Infix.(pure x <*? z))
    in

    law "Distributivity"
      ~lhs:("pure x <*? (y *> z)" =~ lhs)
      ~rhs:("(pure x <*? y) *> (pure x <*? z)" =~ rhs)
  ;;

  let selective_associativity () =
    let lhs x y z = S.(x <*? (y <*? z))
    and rhs x y z =
      let f a = Either.map_right Either.right a in
      let g x a = Either.map ~left:(fun x -> (x, a)) ~right:(fun f -> f a) x in
      let h f (a, b) = f a b in
      let open S in
      let fst = f <$> x
      and snd = g <$> y
      and trd = h <$> z in
      fst <*? snd <*? trd
    in

    law "Associativity" ~lhs:("x <*? (y <*? z)" =~ lhs)
      ~rhs:
        ( "(Either.(map_right right) <$> x) <*? ((fun x a -> Either.map \
           ~left:(fun x -> (x, a)) ~right:(fun f -> f a) x) <$> y) <*? \
           (uncurry <$> z)"
        =~ rhs )
  ;;

  let selective_apply_pure_function_to_the_result () =
    let lhs f x y = S.(f <$> select x y)
    and rhs f x y = S.(select (Either.map_right f <$> x) (( % ) f <$> y)) in

    law "Apply a pure function to the result (Theorem 1)"
      ~lhs:("f <$> select x y" =~ lhs)
      ~rhs:("select (Either.map_right f <$> x) (map f <$> y)" =~ rhs)
  ;;

  let selective_apply_pure_function_to_left_case_of_the_first_argument () =
    let lhs f x y = S.(select (Either.map_left f <$> x) y)
    and rhs f x y = S.(select x (( %> ) f <$> y)) in

    law
      "Apply a pure function to the Left case of the first argument (Theorem 2)"
      ~lhs:("select (Either.map_left f <$> x) y" =~ lhs)
      ~rhs:("select x ((%>) f) <$> y)" =~ rhs)
  ;;

  let selective_apply_pure_function_to_the_second_argument () =
    let lhs f x y = S.(select x (f <$> y))
    and rhs f x y =
      S.(select (Either.map_left (flip f) <$> x) (( |> ) <$> y))
    in

    law "Apply a pure function to the second argument (Theorem 3)"
      ~lhs:("select x (f <$> y)" =~ lhs)
      ~rhs:("select (Either.map_left (flip f) <$> x) ((|>) <$> y)" =~ rhs)
  ;;

  let selective_generalized_identity () =
    let lhs x y = S.(x <*? pure y)
    and rhs x y = S.(Either.fold ~left:y ~right:id <$> x) in

    law "Generalised identity (Theorem 4)" ~lhs:("x <*? pure y" =~ lhs)
      ~rhs:("Either.case y Fun.id <$> x" =~ rhs)
  ;;
end

module For_rigid (S : Preface_specs.SELECTIVE) :
  LAWS_RIGID with module Selective := S = struct
  open Law
  include For (S)

  let selective_rigid_apply () =
    let lhs f x = S.apply f x
    and rhs f x = S.(select (map Either.left f) (map ( |> ) x)) in

    law "Selective Apply for Rigid Selective (Theorem 5)" ~lhs:("f <*> x" =~ lhs)
      ~rhs:("select (map Either.left f) (map ( |> ) x" =~ rhs)
  ;;

  let selective_rigid_interchange () =
    let lhs x y z = S.(Infix.(ignore <$> x) *> Infix.(y <*? z))
    and rhs x y z = S.(Infix.((ignore <$> x) *> y) <*? z) in

    law "Selective Interchange for Rigid Selective (Theorem 6)"
      ~lhs:("x *> (y <*? z)" =~ lhs) ~rhs:("(x *> y) <*? z" =~ rhs)
  ;;
end
