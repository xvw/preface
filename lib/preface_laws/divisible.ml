module type LAWS = sig
  type 'a t

  val divisible_1 : unit -> ('a t, 'a t) Law.t
  val divisible_2 : unit -> ('a t, 'a t) Law.t

  val divisible_3 :
       unit
    -> ( 'a t
       , 'a t -> 'a t -> 'a t )
       Law.t

  val divisible_4 :
    unit -> ('a -> 'b * 'c, 'b t -> 'a t) Law.t

  val divisible_5 :
    unit -> ('a -> 'b * 'c, 'c t -> 'a t) Law.t
end

module For (D : Preface_specs.DIVISIBLE) : LAWS with type 'a t := 'a D.t =
struct
  open Law
  open Preface_core.Fun.Infix
  include Contravariant.For (D)

  let delta x = (x, x)

  let divisible_1 () =
    let lhs m = D.divide delta m D.conquer
    and rhs m = m in

    law ("divide (fun x -> (x, x)) m conquer" =~ lhs) ("m" =~ rhs)
  ;;

  let divisible_2 () =
    let lhs m = D.divide delta D.conquer m
    and rhs m = m in

    law ("divide (fun x -> (x, x)) conquer m" =~ lhs) ("m" =~ rhs)
  ;;

  let divisible_3 () =
    let lhs m n o = D.divide delta (D.divide delta m n) o
    and rhs m n o = D.divide delta m (D.divide delta n o) in

    law
      ("divide (fun x -> (x, x)) (divide delta m n) o" =~ lhs)
      ("divide (fun x -> (x, x)) m (divide delta n o)" =~ rhs)
  ;;

  let divisible_4 () =
    let lhs f m = D.divide f m D.conquer
    and rhs f m = D.contramap (fst % f) m in

    law ("divide f m conquer" =~ lhs) ("contramap (fst % f)" =~ rhs)
  ;;

  let divisible_5 () =
    let lhs f m = D.divide f D.conquer m
    and rhs f m = D.contramap (snd % f) m in

    law ("divide f conquer m" =~ lhs) ("contramap (snd % f)" =~ rhs)
  ;;
end
