module type LAWS_MONOID = sig
  type ('a, 'index) t

  include Indexed_monad.LAWS with type ('a, 'index) t := ('a, 'index) t

  val monad_plus_monoid_1 : unit -> (('a, 'index) t, ('a, 'index) t) Law.t
  val monad_plus_monoid_2 : unit -> (('a, 'index) t, ('a, 'index) t) Law.t

  val monad_plus_monoid_3 :
       unit
    -> ( ('a, 'index) t
       , ('a, 'index) t -> ('a, 'index) t -> ('a, 'index) t )
       Law.t
end

module type LAWS_LEFT_ABSORPTION = sig
  type ('a, 'index) t

  include Indexed_monad.LAWS with type ('a, 'index) t := ('a, 'index) t

  val monad_plus_left_absorb_1 :
    unit -> ('a -> ('b, 'index) t, ('b, 'index) t) Law.t
end

module type LAWS_LEFT_DISTRIBUTIVITY = sig
  type ('a, 'index) t

  include Indexed_monad.LAWS with type ('a, 'index) t := ('a, 'index) t

  val monad_plus_left_distrib_1 :
       unit
    -> ( ('a, 'index) t
       , ('a, 'index) t -> ('a -> ('b, 'index) t) -> ('b, 'index) t )
       Law.t
end

module type LAWS_LEFT_CATCH = sig
  type ('a, 'index) t

  include Indexed_monad.LAWS with type ('a, 'index) t := ('a, 'index) t

  val monad_plus_left_catch_1 :
    unit -> ('a, ('a, 'index) t -> ('a, 'index) t) Law.t
end

module For_monoidal (M : Preface_specs.INDEXED_MONAD_PLUS) :
  LAWS_MONOID with type ('a, 'index) t := ('a, 'index) M.t = struct
  open Law
  include Indexed_monad.For (M)

  let monad_plus_monoid_1 () =
    let lhs = M.(combine neutral)
    and rhs x = x in

    law ("neutral <|> x" =~ lhs) ("x" =~ rhs)
  ;;

  let monad_plus_monoid_2 () =
    let lhs x = M.(combine x neutral)
    and rhs x = x in

    law ("x <|> neutral" =~ lhs) ("x" =~ rhs)
  ;;

  let monad_plus_monoid_3 () =
    let lhs a b c = M.(Infix.(a <|> b) <|> c)
    and rhs a b c = M.(a <|> Infix.(b <|> c)) in

    law ("(a <|> b) <|> c" =~ lhs) ("a <|> (b <|> c)" =~ rhs)
  ;;
end

module For_left_absorption (M : Preface_specs.INDEXED_MONAD_PLUS) :
  LAWS_LEFT_ABSORPTION with type ('a, 'index) t := ('a, 'index) M.t = struct
  include Indexed_monad.For (M)
  open Law

  let monad_plus_left_absorb_1 () =
    let lhs f = M.(neutral >>= f)
    and rhs _ = M.neutral in

    law ("neutral >>= f" =~ lhs) ("neutral" =~ rhs)
  ;;
end

module For_left_distributivity (M : Preface_specs.INDEXED_MONAD_PLUS) :
  LAWS_LEFT_DISTRIBUTIVITY with type ('a, 'index) t := ('a, 'index) M.t = struct
  include Indexed_monad.For (M)
  open Law

  let monad_plus_left_distrib_1 () =
    let lhs a b f = M.(a <|> b >>= f)
    and rhs a b f = M.(a >>= f <|> (b >>= f)) in

    law ("(a <|> b) >>= f" =~ lhs) ("(a >>= f) <|> (b >>= f)" =~ rhs)
  ;;
end

module For_left_catch (M : Preface_specs.INDEXED_MONAD_PLUS) :
  LAWS_LEFT_CATCH with type ('a, 'index) t := ('a, 'index) M.t = struct
  include Indexed_monad.For (M)
  open Law

  let monad_plus_left_catch_1 () =
    let lhs a b = M.(return a <|> b)
    and rhs a _ = M.(return a) in

    law ("(return a) <|> b" =~ lhs) ("return a" =~ rhs)
  ;;
end
