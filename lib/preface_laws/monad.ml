module type LAWS = sig
  module Monad : Preface_specs.MONAD
  include Bind.LAWS with module Bind := Monad

  val monad_join_map_return_and_join_return_are_id :
    unit -> ('a Monad.t, 'a Monad.t * 'a Monad.t) Law.t

  val monad_natural_transformation : unit -> ('a -> 'b, 'a -> 'b Monad.t) Law.t

  val monad_bind_left_identity :
    unit -> ('a, ('a -> 'b Monad.t) -> 'b Monad.t) Law.t

  val monad_bind_right_identity : unit -> ('a Monad.t, 'a Monad.t) Law.t

  val monad_kleisli_composition_left_identity :
    unit -> ('a, ('a -> 'b Monad.t) -> 'b Monad.t) Law.t

  val monad_kleisli_composition_right_identity :
    unit -> ('a, ('a -> 'b Monad.t) -> 'b Monad.t) Law.t
end

module For (M : Preface_specs.MONAD) : LAWS with module Monad := M = struct
  include Bind.For (M)
  open Preface_core.Fun
  open Law

  let monad_join_map_return_and_join_return_are_id () =
    let lhs x = (M.(join % map return) x, id x)
    and rhs x = (M.(join % return) x, id x) in

    law "Join % map return is join % return which is identity"
      ~lhs:("join % map return = id" =~ lhs)
      ~rhs:("join % return = id" =~ rhs)
  ;;

  let monad_natural_transformation () =
    let lhs f x = M.(map f % return) x
    and rhs f x = M.(return % f) x in

    law "Map with return form a natural transformation"
      ~lhs:("map f % return" =~ lhs) ~rhs:("return % f" =~ rhs)
  ;;

  let monad_bind_left_identity () =
    let lhs x f = M.(return x >>= f)
    and rhs x f = f x in

    law "(>>=) left identity" ~lhs:("return x >>= f" =~ lhs) ~rhs:("f x" =~ rhs)
  ;;

  let monad_bind_right_identity () =
    let lhs x = M.(x >>= return)
    and rhs x = x in

    law "(>>=) right identity" ~lhs:("x >>= return" =~ lhs) ~rhs:("x" =~ rhs)
  ;;

  let monad_kleisli_composition_left_identity () =
    let lhs x f = M.(return >=> f) x
    and rhs x f = f x in

    law "(>=>) left identity" ~lhs:("return >=> f" =~ lhs) ~rhs:("f" =~ rhs)
  ;;

  let monad_kleisli_composition_right_identity () =
    let lhs x f = M.(f >=> return) x
    and rhs x f = f x in

    law "(>=>) right identity" ~lhs:("f >=> return" =~ lhs) ~rhs:("f" =~ rhs)
  ;;
end
