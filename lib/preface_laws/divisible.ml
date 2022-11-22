module type LAWS = sig
  module Divisible : Preface_specs.DIVISIBLE
  include Contravariant.LAWS with module Contravariant := Divisible

  val divisible_left_identity : unit -> ('a Divisible.t, 'a Divisible.t) Law.t
  val divisible_right_identity : unit -> ('a Divisible.t, 'a Divisible.t) Law.t

  val divisible_associativity :
       unit
    -> ( 'a Divisible.t
       , 'a Divisible.t -> 'a Divisible.t -> 'a Divisible.t )
       Law.t

  val divisible_left_identity_using_fst :
    unit -> ('a -> 'b * 'c, 'b Divisible.t -> 'a Divisible.t) Law.t

  val divisible_right_identity_using_snd :
    unit -> ('a -> 'b * 'c, 'c Divisible.t -> 'a Divisible.t) Law.t

  val divisible_associativity_using_id :
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

  let divisible_left_identity () =
    let lhs m = D.divide delta m D.conquer
    and rhs m = m in

    law "left identity"
      ~lhs:("divide (fun x -> (x, x)) m conquer" =~ lhs)
      ~rhs:("m" =~ rhs)
  ;;

  let divisible_right_identity () =
    let lhs m = D.divide delta D.conquer m
    and rhs m = m in

    law "right identity"
      ~lhs:("divide (fun x -> (x, x)) conquer m" =~ lhs)
      ~rhs:("m" =~ rhs)
  ;;

  let divisible_associativity () =
    let lhs m n o = D.divide delta (D.divide delta m n) o
    and rhs m n o = D.divide delta m (D.divide delta n o) in

    law "associativity"
      ~lhs:("divide (fun x -> (x, x)) (divide delta m n) o" =~ lhs)
      ~rhs:("divide (fun x -> (x, x)) m (divide delta n o)" =~ rhs)
  ;;

  let divisible_left_identity_using_fst () =
    let lhs f m = D.divide f m D.conquer
    and rhs f m = D.contramap (fst % f) m in

    law "left identity using fst"
      ~lhs:("divide f m conquer" =~ lhs)
      ~rhs:("contramap (fst % f)" =~ rhs)
  ;;

  let divisible_right_identity_using_snd () =
    let lhs f m = D.divide f D.conquer m
    and rhs f m = D.contramap (snd % f) m in

    law "right identity using snd"
      ~lhs:("divide f conquer m" =~ lhs)
      ~rhs:("contramap (snd % f)" =~ rhs)
  ;;

  let divisible_associativity_using_id () =
    let lhs f g m n o = D.divide f (D.divide g m n) o
    and rhs f g m n o =
      let f' a =
        let bc, _ = f a in
        let b, c = g bc in
        (a, (b, c))
      in
      D.divide f' m (D.divide (fun x -> x) n o)
    in

    law "associativity using id"
      ~lhs:("divide f (divide g m n) o" =~ lhs)
      ~rhs:
        ( "divide (fun a -> let bc = fst (f a) in let (b, c) = g bc in (a, (b, \
           c))) m (divide (fun x -> x) n o)"
        =~ rhs )
  ;;
end
