module type LAWS_MONOID = sig
  type ('a, 'index) t

  include Indexed_applicative.LAWS with type ('a, 'index) t := ('a, 'index) t

  val alternative_monoid_1 : unit -> (('a, 'index) t, ('a, 'index) t) Law.t
  val alternative_monoid_2 : unit -> (('a, 'index) t, ('a, 'index) t) Law.t

  val alternative_monoid_3 :
       unit
    -> ( ('a, 'index) t
       , ('a, 'index) t -> ('a, 'index) t -> ('a, 'index) t )
       Law.t
end

module type LAWS_RIGHT_DISTRIBUTIVITY = sig
  type ('a, 'index) t

  include Indexed_applicative.LAWS with type ('a, 'index) t := ('a, 'index) t

  val alternative_right_distrib_1 :
       unit
    -> ( ('a -> 'b, 'index) t
       , ('a -> 'b, 'index) t -> ('a, 'index) t -> ('b, 'index) t )
       Law.t
end

module type LAWS_RIGHT_ABSORPTION = sig
  type ('a, 'index) t

  include Indexed_applicative.LAWS with type ('a, 'index) t := ('a, 'index) t

  val alternative_right_absorb_1 :
    unit -> (('a, 'index) t, ('a, 'index) t) Law.t
end

module For_monoidal (A : Preface_specs.INDEXED_ALTERNATIVE) :
  LAWS_MONOID with type ('a, 'index) t := ('a, 'index) A.t = struct
  open Law
  include Indexed_applicative.For (A)

  let alternative_monoid_1 () =
    let lhs = A.(combine neutral)
    and rhs x = x in

    law ("neutral <|> x" =~ lhs) ("x" =~ rhs)
  ;;

  let alternative_monoid_2 () =
    let lhs x = A.(combine x neutral)
    and rhs x = x in

    law ("x <|> neutral" =~ lhs) ("x" =~ rhs)
  ;;

  let alternative_monoid_3 () =
    let lhs a b c = A.(Infix.(a <|> b) <|> c)
    and rhs a b c = A.(a <|> Infix.(b <|> c)) in

    law ("(a <|> b) <|> c" =~ lhs) ("a <|> (b <|> c)" =~ rhs)
  ;;
end

module For_right_distributivity (A : Preface_specs.INDEXED_ALTERNATIVE) :
  LAWS_RIGHT_DISTRIBUTIVITY with type ('a, 'index) t := ('a, 'index) A.t =
struct
  open Law
  include Indexed_applicative.For (A)

  let alternative_right_distrib_1 () =
    let lhs f g x = A.(Infix.(f <|> g) <*> x)
    and rhs f g x = A.(Infix.(f <*> x) <|> Infix.(g <*> x)) in

    law ("(f <|> g) <*> a" =~ lhs) ("(f <*> a) <|> (g <*> a)" =~ rhs)
  ;;
end

module For_right_absorbtion (A : Preface_specs.INDEXED_ALTERNATIVE) :
  LAWS_RIGHT_ABSORPTION with type ('a, 'index) t := ('a, 'index) A.t = struct
  open Law
  include Indexed_applicative.For (A)

  let alternative_right_absorb_1 () =
    let lhs = A.(apply neutral)
    and rhs _ = A.neutral in

    law ("neutral <*> x" =~ lhs) ("neutral" =~ rhs)
  ;;
end
