module type LAWS = sig
  type 'a t

  val monad_1 : unit -> ('a t, 'a t * 'a t) Law.t
  val monad_2 : unit -> ('a -> 'b, 'a -> 'b t) Law.t
  val monad_3 : unit -> ('a, ('a -> 'b t) -> 'b t) Law.t
  val monad_4 : unit -> ('a t, 'a t) Law.t
  val monad_5 : unit -> ('a, ('a -> 'b t) -> 'b t) Law.t
  val monad_6 : unit -> ('a, ('a -> 'b t) -> 'b t) Law.t
end

module For (M : Preface_specs.MONAD) : LAWS with type 'a t := 'a M.t = struct
  include Bind.For (M)
  open Preface_core.Fun
  open Law

  let monad_1 () =
    let lhs x = (M.(join % map return) x, id x)
    and rhs x = (M.(join % return) x, id x) in

    law ("join % map return = id" =~ lhs) ("join % return" =~ rhs)
  ;;

  let monad_2 () =
    let lhs f x = M.(map f % return) x
    and rhs f x = M.(return % f) x in

    law ("map f % return" =~ lhs) ("return % f" =~ rhs)
  ;;

  let monad_3 () =
    let lhs x f = M.(return x >>= f)
    and rhs x f = f x in

    law ("return x >>= f" =~ lhs) ("f x" =~ rhs)
  ;;

  let monad_4 () =
    let lhs x = M.(x >>= return)
    and rhs x = x in

    law ("x >>= return" =~ lhs) ("x" =~ rhs)
  ;;

  let monad_5 () =
    let lhs x f = M.(return >=> f) x
    and rhs x f = f x in

    law ("return >=> f" =~ lhs) ("f" =~ rhs)
  ;;

  let monad_6 () =
    let lhs x f = M.(f >=> return) x
    and rhs x f = f x in

    law ("f >=> return" =~ lhs) ("f" =~ rhs)
  ;;
end
