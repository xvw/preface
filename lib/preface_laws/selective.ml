module type LAWS = sig
  type 'a t

  val selective_1 :
    unit -> (('a, 'a) Either.t t, 'a t) Law.t

  val selective_2 :
       unit
    -> ( ('a, 'b) Either.t
       , ('a -> 'b) t -> ('a -> 'b) t -> 'b t )
       Law.t

  val selective_3 :
       unit
    -> ( ('a, 'b) Either.t t
       ,    ('c, 'a -> 'b) Either.t t
         -> ('c -> 'a -> 'b) t
         -> 'b t )
       Law.t

  val selective_4 :
       unit
    -> ( 'a -> 'b
       ,    ('c, 'a) Either.t t
         -> ('c -> 'a) t
         -> 'b t )
       Law.t

  val selective_5 :
       unit
    -> ( 'a -> 'b
       ,    ('a, 'c) Either.t t
         -> ('b -> 'c) t
         -> 'c t )
       Law.t

  val selective_6 :
       unit
    -> ( 'a -> 'b -> 'c
       , ('b, 'c) Either.t t -> 'a t -> 'c t )
       Law.t

  val selective_7 :
    unit -> (('a, 'b) Either.t t, ('a -> 'b) -> 'b t) Law.t
end

module type LAWS_RIGID = sig
  include LAWS

  val selective_8 :
    unit -> (('a -> 'b) t, 'a t -> 'b t) Law.t

  val selective_9 :
       unit
    -> ( 'a t
       ,    ('b, 'c) Either.t t
         -> ('b -> 'c) t
         -> 'c t )
       Law.t
end

module For (S : Preface_specs.SELECTIVE) : LAWS with type 'a t := 'a S.t =
struct
  open Law
  open Preface_core.Fun
  include Applicative.For (S)

  let selective_1 () =
    let lhs x = S.(x <*? pure id)
    and rhs x = S.(Either.fold ~left:id ~right:id <$> x) in

    law ("x <*? pure Fun.id" =~ lhs) ("Either.case Fun.id Fun.id <$> x" =~ rhs)
  ;;

  let selective_2 () =
    let lhs x y z = S.(pure x <*? Infix.(replace () y *> z))
    and rhs x y z =
      S.(replace () Infix.(pure x <*? y) *> Infix.(pure x <*? z))
    in

    law
      ("pure x <*? (y *> z)" =~ lhs)
      ("(pure x <*? y) *> (pure x <*? z)" =~ rhs)
  ;;

  let selective_3 () =
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

    law ("x <*? (y <*? z)" =~ lhs)
      ( "(Either.(map_right right) <$> x) <*? ((fun x a -> Either.map \
         ~left:(fun x -> (x, a)) ~right:(fun f -> f a) x) <$> y) <*? (uncurry \
         <$> z)"
      =~ rhs )
  ;;

  let selective_4 () =
    let lhs f x y = S.(f <$> select x y)
    and rhs f x y = S.(select (Either.map_right f <$> x) (( % ) f <$> y)) in

    law
      ("f <$> select x y" =~ lhs)
      ("select (Either.map_right f <$> x) (map f <$> y)" =~ rhs)
  ;;

  let selective_5 () =
    let lhs f x y = S.(select (Either.map_left f <$> x) y)
    and rhs f x y = S.(select x (( %> ) f <$> y)) in

    law
      ("select (Either.map_left f <$> x) y" =~ lhs)
      ("select x ((%>) f) <$> y)" =~ rhs)
  ;;

  let selective_6 () =
    let lhs f x y = S.(select x (f <$> y))
    and rhs f x y =
      S.(select (Either.map_left (flip f) <$> x) (( |> ) <$> y))
    in

    law
      ("select x (f <$> y)" =~ lhs)
      ("select (Either.map_left (flip f) <$> x) ((|>) <$> y)" =~ rhs)
  ;;

  let selective_7 () =
    let lhs x y = S.(x <*? pure y)
    and rhs x y = S.(Either.fold ~left:y ~right:id <$> x) in

    law ("x <*? pure y" =~ lhs) ("Either.case y Fun.id <$> x" =~ rhs)
  ;;
end

module For_rigid (S : Preface_specs.SELECTIVE) :
  LAWS_RIGID with type 'a t := 'a S.t = struct
  open Law
  include For (S)

  let selective_8 () =
    let lhs f x = S.apply f x
    and rhs f x = S.(select (map Either.left f) (map ( |> ) x)) in

    law ("f <*> x" =~ lhs) ("select (map Either.left f) (map ( |> ) x" =~ rhs)
  ;;

  let selective_9 () =
    let lhs x y z = S.(Infix.(ignore <$> x) *> Infix.(y <*? z))
    and rhs x y z = S.(Infix.((ignore <$> x) *> y) <*? z) in

    law ("x *> (y <*? z)" =~ lhs) ("(x *> y) <*? z" =~ rhs)
  ;;
end
