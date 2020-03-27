(** TODO *)

module Free (Functor : Preface_specs.FUNCTOR) = struct
  module Functor = Functor

  type 'a t =
    | Return : 'a -> 'a t
    | Bind : 'a t Functor.t -> 'a t

  let eta f =
    let open Preface_core.Fun in
    compose_right_to_left (fun a -> Bind a) (Functor.map (fun a -> Return a)) f
  ;;
end

module Free_functor (Free : Preface_specs.FREE) = Functor.Via_map (struct
  type 'a t = 'a Free.t

  let rec map f =
    let open Free in
    function
    | Return v -> Return (f v) | Bind f' -> Bind (Functor.map (map f) f')
  ;;
end)

module Free_applicative (Free : Preface_specs.FREE) =
Applicative.Via_apply (struct
  include Free_functor (Free)

  let pure a = Free.(Return a)

  let rec apply f a =
    let open Free in
    match f with
    | Return f' -> map f' a
    | Bind f' -> Bind (Functor.map (fun f -> apply f a) f')
  ;;
end)

module Free_monad (Free : Preface_specs.FREE) = Monad.Via_bind (struct
  include Free_functor (Free)
  include Free_applicative (Free)

  let return = pure

  let rec bind f =
    Free.((function Return a -> f a | Bind a -> Bind (Functor.map (bind f) a)))
  ;;
end)
