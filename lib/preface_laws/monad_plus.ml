module type LAWS_MONOID = sig
  module Monad_plus : Preface_specs.MONAD_PLUS
  include Monad.LAWS with module Monad := Monad_plus

  val monad_plus_left_identity :
    unit -> ('a Monad_plus.t, 'a Monad_plus.t) Law.t

  val monad_plus_right_identity :
    unit -> ('a Monad_plus.t, 'a Monad_plus.t) Law.t

  val monad_plus_associative_combine :
       unit
    -> ( 'a Monad_plus.t
       , 'a Monad_plus.t -> 'a Monad_plus.t -> 'a Monad_plus.t )
       Law.t
end

module type LAWS_LEFT_ABSORPTION = sig
  module Monad_plus : Preface_specs.MONAD_PLUS
  include Monad.LAWS with module Monad := Monad_plus

  val monad_plus_left_absorption :
    unit -> ('a -> 'b Monad_plus.t, 'b Monad_plus.t) Law.t
end

module type LAWS_LEFT_DISTRIBUTIVITY = sig
  module Monad_plus : Preface_specs.MONAD_PLUS
  include Monad.LAWS with module Monad := Monad_plus

  val monad_plus_bind_left_distribution :
       unit
    -> ( 'a Monad_plus.t
       , 'a Monad_plus.t -> ('a -> 'b Monad_plus.t) -> 'b Monad_plus.t )
       Law.t
end

module type LAWS_LEFT_CATCH = sig
  module Monad_plus : Preface_specs.MONAD_PLUS
  include Monad.LAWS with module Monad := Monad_plus

  val monad_plus_left_catch :
    unit -> ('a, 'a Monad_plus.t -> 'a Monad_plus.t) Law.t
end

module For_monoidal (M : Preface_specs.MONAD_PLUS) :
  LAWS_MONOID with module Monad_plus := M = struct
  open Law
  include Monad.For (M)

  let monad_plus_left_identity () =
    let lhs = M.(combine neutral)
    and rhs x = x in

    law "Left identity" ~lhs:("neutral <|> x" =~ lhs) ~rhs:("x" =~ rhs)
  ;;

  let monad_plus_right_identity () =
    let lhs x = M.(combine x neutral)
    and rhs x = x in

    law "Right identity" ~lhs:("x <|> neutral" =~ lhs) ~rhs:("x" =~ rhs)
  ;;

  let monad_plus_associative_combine () =
    let lhs a b c = M.(Infix.(a <|> b) <|> c)
    and rhs a b c = M.(a <|> Infix.(b <|> c)) in

    law "Combine must be associative" ~lhs:("(a <|> b) <|> c" =~ lhs)
      ~rhs:("a <|> (b <|> c)" =~ rhs)
  ;;
end

module For_left_absorption (M : Preface_specs.MONAD_PLUS) :
  LAWS_LEFT_ABSORPTION with module Monad_plus := M = struct
  include Monad.For (M)
  open Law

  let monad_plus_left_absorption () =
    let lhs f = M.(neutral >>= f)
    and rhs _ = M.neutral in

    law "Neutral is left absorbant" ~lhs:("neutral >>= f" =~ lhs)
      ~rhs:("neutral" =~ rhs)
  ;;
end

module For_left_distributivity (M : Preface_specs.MONAD_PLUS) :
  LAWS_LEFT_DISTRIBUTIVITY with module Monad_plus := M = struct
  include Monad.For (M)
  open Law

  let monad_plus_bind_left_distribution () =
    let lhs a b f = M.(a <|> b >>= f)
    and rhs a b f = M.(a >>= f <|> (b >>= f)) in

    law "Left distribution of (>>=) over (<|>)" ~lhs:("(a <|> b) >>= f" =~ lhs)
      ~rhs:("(a >>= f) <|> (b >>= f)" =~ rhs)
  ;;
end

module For_left_catch (M : Preface_specs.MONAD_PLUS) :
  LAWS_LEFT_CATCH with module Monad_plus := M = struct
  include Monad.For (M)
  open Law

  let monad_plus_left_catch () =
    let lhs a b = M.(return a <|> b)
    and rhs a _ = M.(return a) in

    law "Left catch  of return over (<|>)"
      ~lhs:("(return a) <|> b" =~ lhs)
      ~rhs:("return a" =~ rhs)
  ;;
end
