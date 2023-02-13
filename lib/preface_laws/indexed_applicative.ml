module type LAWS = sig
  type ('a, 'index) t

  include Indexed_apply.LAWS with type ('a, 'index) t := ('a, 'index) t

  val applicative_1 : unit -> (('a, 'index) t, ('a, 'index) t) Law.t
  val applicative_2 : unit -> ('a -> 'b, 'a -> ('b, 'index) t) Law.t
  val applicative_3 : unit -> (('a -> 'b, 'index) t, 'a -> ('b, 'index) t) Law.t

  val applicative_4 :
       unit
    -> ( ('a -> 'b, 'index) t
       , ('c -> 'a, 'index) t -> ('c, 'index) t -> ('b, 'index) t )
       Law.t

  val applicative_5 : unit -> ('a -> 'b, ('a, 'index) t -> ('b, 'index) t) Law.t
end

module For (A : Preface_specs.INDEXED_APPLICATIVE) :
  LAWS with type ('a, 'index) t := ('a, 'index) A.t = struct
  open Law
  open Preface_core.Fun.Infix
  include Indexed_apply.For (A)

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
