module type LAWS_MONOID = sig
  module Alternative : Preface_specs.ALTERNATIVE
  include Applicative.LAWS with module Applicative := Alternative

  val alternative_left_identity :
    unit -> ('a Alternative.t, 'a Alternative.t) Law.t

  val alternative_right_identity :
    unit -> ('a Alternative.t, 'a Alternative.t) Law.t

  val alternative_associative_combine :
       unit
    -> ( 'a Alternative.t
       , 'a Alternative.t -> 'a Alternative.t -> 'a Alternative.t )
       Law.t
end

module type LAWS_RIGHT_DISTRIBUTIVITY = sig
  module Alternative : Preface_specs.ALTERNATIVE
  include Applicative.LAWS with module Applicative := Alternative

  val alternative_apply_right_distribution :
       unit
    -> ( ('a -> 'b) Alternative.t
       , ('a -> 'b) Alternative.t -> 'a Alternative.t -> 'b Alternative.t )
       Law.t
end

module type LAWS_RIGHT_ABSORPTION = sig
  module Alternative : Preface_specs.ALTERNATIVE
  include Applicative.LAWS with module Applicative := Alternative

  val alternative_right_absorption_for_apply :
    unit -> ('a Alternative.t, 'a Alternative.t) Law.t
end

module For_monoidal (A : Preface_specs.ALTERNATIVE) :
  LAWS_MONOID with module Alternative := A = struct
  open Law
  include Applicative.For (A)

  let alternative_left_identity () =
    let lhs = A.(combine neutral)
    and rhs x = x in

    law "Left identity" ~lhs:("neutral <|> x" =~ lhs) ~rhs:("x" =~ rhs)
  ;;

  let alternative_right_identity () =
    let lhs x = A.(combine x neutral)
    and rhs x = x in

    law "Right identity" ~lhs:("x <|> neutral" =~ lhs) ~rhs:("x" =~ rhs)
  ;;

  let alternative_associative_combine () =
    let lhs a b c = A.(Infix.(a <|> b) <|> c)
    and rhs a b c = A.(a <|> Infix.(b <|> c)) in

    law "Combine must be associative" ~lhs:("(a <|> b) <|> c" =~ lhs)
      ~rhs:("a <|> (b <|> c)" =~ rhs)
  ;;
end

module For_right_distributivity (A : Preface_specs.ALTERNATIVE) :
  LAWS_RIGHT_DISTRIBUTIVITY with module Alternative := A = struct
  open Law
  include Applicative.For (A)

  let alternative_apply_right_distribution () =
    let lhs f g x = A.(Infix.(f <|> g) <*> x)
    and rhs f g x = A.(Infix.(f <*> x) <|> Infix.(g <*> x)) in

    law "Apply is right distributive" ~lhs:("(f <|> g) <*> a" =~ lhs)
      ~rhs:("(f <*> a) <|> (g <*> a)" =~ rhs)
  ;;
end

module For_right_absorbtion (A : Preface_specs.ALTERNATIVE) :
  LAWS_RIGHT_ABSORPTION with module Alternative := A = struct
  open Law
  include Applicative.For (A)

  let alternative_right_absorption_for_apply () =
    let lhs = A.(apply neutral)
    and rhs = Fun.id in

    law "Right absorbtion for Apply" ~lhs:("neutral <*> x" =~ lhs)
      ~rhs:("x" =~ rhs)
  ;;
end
