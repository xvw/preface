module type LAWS = sig
  module Applicative : Preface_specs.APPLICATIVE
  include Apply.LAWS with module Apply := Applicative

  val applicative_identity : unit -> ('a Applicative.t, 'a Applicative.t) Law.t

  val applicative_homomorphism :
    unit -> ('a -> 'b, 'a -> 'b Applicative.t) Law.t

  val applicative_interchange :
    unit -> (('a -> 'b) Applicative.t, 'a -> 'b Applicative.t) Law.t

  val applicative_composition :
       unit
    -> ( ('a -> 'b) Applicative.t
       , ('c -> 'a) Applicative.t -> 'c Applicative.t -> 'b Applicative.t )
       Law.t

  val applicative_map_is_pure_and_apply :
    unit -> ('a -> 'b, 'a Applicative.t -> 'b Applicative.t) Law.t
end

module For (A : Preface_specs.APPLICATIVE) : LAWS with module Applicative := A =
struct
  open Law
  open Preface_core.Fun.Infix
  include Apply.For (A)

  let applicative_identity () =
    let lhs x = A.(pure Fun.id <*> x)
    and rhs x = x in

    law "Identity" ~lhs:("pure id <*> x" =~ lhs) ~rhs:("x" =~ rhs)
  ;;

  let applicative_homomorphism () =
    let lhs f x = A.(pure f <*> pure x)
    and rhs f x = A.pure (f x) in

    law "Homomorphism" ~lhs:("pure f <*> pure x" =~ lhs) ~rhs:("pure f x" =~ rhs)
  ;;

  let applicative_interchange () =
    let lhs f x = A.(f <*> pure x)
    and rhs f x = A.(pure (( |> ) x) <*> f) in

    law "Interchange" ~lhs:("f <*> pure x" =~ lhs)
      ~rhs:("pure ((|>) x) <*> f" =~ rhs)
  ;;

  let applicative_composition () =
    let lhs u v w = A.(pure ( % ) <*> u <*> v <*> w)
    and rhs u v w = A.(u <*> Infix.(v <*> w)) in

    law "Composition"
      ~lhs:("pure ( % ) <*> u <*> v <*> w" =~ lhs)
      ~rhs:("u <*> (v <*> w)" =~ rhs)
  ;;

  let applicative_map_is_pure_and_apply () =
    let lhs = A.map
    and rhs f x = A.(pure f <*> x) in

    law "Map is encodable via pure and apply" ~lhs:("map f x" =~ lhs)
      ~rhs:("pure f <*> x" =~ rhs)
  ;;
end
