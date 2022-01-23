open Preface_core.Fun.Infix

module Over (Type : Preface_specs.Types.T1) = struct
  type 'a f = 'a Type.t

  type _ t =
    | Return : 'a -> 'a t
    | Bind : 'b f * ('b -> 'a t) -> 'a t

  let perform f = Bind (f, fun a -> Return a)

  type ('a, 'b) handle = ('a -> 'b) -> 'a f -> 'b
  type 'a handler = { handler : 'b. ('b, 'a) handle }

  let run f =
    let rec loop_run = function
      | Return a -> a
      | Bind (intermediate, continue) ->
        let k x = loop_run (continue x) in
        f.handler k intermediate
    in
    loop_run
  ;;

  let rec map f = function
    | Return x -> Return (f x)
    | Bind (i, c) -> Bind (i, c %> map f)
  ;;

  module Functor = Functor.Via_map (struct
    type nonrec 'a t = 'a t

    let map = map
  end)

  module Applicative = Applicative.Via_apply (struct
    type nonrec 'a t = 'a t

    let pure a = Return a

    let rec apply f a =
      match f with
      | Return f' -> map f' a
      | Bind (i, c) -> Bind (i, c %> fun f -> apply f a)
    ;;
  end)

  module Monad = Monad.Via_bind (struct
    type nonrec 'a t = 'a t

    let return a = Return a

    let rec bind f = function
      | Return a -> f a
      | Bind (i, c) -> Bind (i, c %> bind f)
    ;;
  end)

  module Selective =
    Selective.Over_applicative_via_select
      (Applicative)
      (Selective.Select_from_monad (Monad))

  include (Monad : Preface_specs.MONAD with type 'a t := 'a t)
end
