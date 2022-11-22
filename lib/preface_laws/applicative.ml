module type LAWS = sig
  module Applicative : Preface_specs.APPLICATIVE
  include Apply.LAWS with module Apply := Applicative

  val applicative_1 : unit -> ('a Applicative.t, 'a Applicative.t) Law.t
  val applicative_2 : unit -> ('a -> 'b, 'a -> 'b Applicative.t) Law.t

  val applicative_3 :
    unit -> (('a -> 'b) Applicative.t, 'a -> 'b Applicative.t) Law.t

  val applicative_4 :
       unit
    -> ( ('a -> 'b) Applicative.t
       , ('c -> 'a) Applicative.t -> 'c Applicative.t -> 'b Applicative.t )
       Law.t

  val applicative_5 :
    unit -> ('a -> 'b, 'a Applicative.t -> 'b Applicative.t) Law.t
end

module For (A : Preface_specs.APPLICATIVE) : LAWS with module Applicative := A =
struct
  open Law
  open Preface_core.Fun.Infix
  include Apply.For (A)

  let applicative_1 () =
    let lhs x = A.(pure Fun.id <*> x)
    and rhs x = x in

    law ("pure id <*> x" =~ lhs) ("x" =~ rhs)
  ;;

  let applicative_2 () =
    let lhs f x = A.(pure f <*> pure x)
    and rhs f x = A.pure (f x) in

    law ("pure f <*> pure x" =~ lhs) ("pure f x" =~ rhs)
  ;;

  let applicative_3 () =
    let lhs f x = A.(f <*> pure x)
    and rhs f x = A.(pure (( |> ) x) <*> f) in

    law ("f <*> pure x" =~ lhs) ("pure ((|>) x) <*> f" =~ rhs)
  ;;

  let applicative_4 () =
    let lhs u v w = A.(pure ( % ) <*> u <*> v <*> w)
    and rhs u v w = A.(u <*> Infix.(v <*> w)) in

    law ("pure ( % ) <*> u <*> v <*> w" =~ lhs) ("u <*> (v <*> w)" =~ rhs)
  ;;

  let applicative_5 () =
    let lhs = A.map
    and rhs f x = A.(pure f <*> x) in

    law ("map f x" =~ lhs) ("pure f <*> x" =~ rhs)
  ;;
end
