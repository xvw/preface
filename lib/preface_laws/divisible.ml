module type LAWS = sig
  module Divisible : Preface_specs.DIVISIBLE
  include Contravariant.LAWS with module Contravariant := Divisible

  val divisible_1 : unit -> ('a Divisible.t, 'a Divisible.t) Law.t
  val divisible_2 : unit -> ('a Divisible.t, 'a Divisible.t) Law.t

  val divisible_3 :
       unit
    -> ( 'a Divisible.t
       , 'a Divisible.t -> 'a Divisible.t -> 'a Divisible.t )
       Law.t

  val divisible_4 :
    unit -> ('a -> 'b * 'c, 'b Divisible.t -> 'a Divisible.t) Law.t

  val divisible_5 :
    unit -> ('a -> 'b * 'c, 'c Divisible.t -> 'a Divisible.t) Law.t

  val divisible_6 :
       unit
    -> ( 'a -> 'b * 'a
       ,    ('b -> 'a * 'a)
         -> 'a Divisible.t
         -> 'a Divisible.t
         -> 'a Divisible.t
         -> 'a Divisible.t )
       Law.t
end

module For (D : Preface_specs.DIVISIBLE) : LAWS with module Divisible := D =
struct
  open Law
  open Preface_core.Fun.Infix
  include Contravariant.For (D)

  let delta x = (x, x)

  let divisible_1 () =
    let lhs m = D.divide delta m D.conquer
    and rhs m = m in

    law ~lhs:("divide (fun x -> (x, x)) m conquer" =~ lhs) ~rhs:("m" =~ rhs)
  ;;

  let divisible_2 () =
    let lhs m = D.divide delta D.conquer m
    and rhs m = m in

    law ~lhs:("divide (fun x -> (x, x)) conquer m" =~ lhs) ~rhs:("m" =~ rhs)
  ;;

  let divisible_3 () =
    let lhs m n o = D.divide delta (D.divide delta m n) o
    and rhs m n o = D.divide delta m (D.divide delta n o) in

    law
      ~lhs:("divide (fun x -> (x, x)) (divide delta m n) o" =~ lhs)
      ~rhs:("divide (fun x -> (x, x)) m (divide delta n o)" =~ rhs)
  ;;

  let divisible_4 () =
    let lhs f m = D.divide f m D.conquer
    and rhs f m = D.contramap (fst % f) m in

    law ~lhs:("divide f m conquer" =~ lhs) ~rhs:("contramap (fst % f)" =~ rhs)
  ;;

  let divisible_5 () =
    let lhs f m = D.divide f D.conquer m
    and rhs f m = D.contramap (snd % f) m in

    law ~lhs:("divide f conquer m" =~ lhs) ~rhs:("contramap (snd % f)" =~ rhs)
  ;;

  let divisible_6 () =
    let lhs f g m n o = D.divide f (D.divide g m n) o
    and rhs f g m n o =
      let f' a =
        let bc, _ = f a in
        let b, c = g bc in
        (a, (b, c))
      in
      D.divide f' m (D.divide (fun x -> x) n o)
    in

    law
      ~lhs:("divide f (divide g m n) o" =~ lhs)
      ~rhs:
        ( "divide (fun a -> let bc = fst (f a) in let (b, c) = g bc in (a, (b, \
           c))) m (divide (fun x -> x) n o)"
        =~ rhs )
  ;;
end
