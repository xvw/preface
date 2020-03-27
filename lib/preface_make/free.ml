(** TODO *)

module Free (F : Preface_specs.Functor.CORE) = struct
  open Preface_core.Fun
  module F = F

  type 'a t =
    | Return : 'a -> 'a t
    | Bind : 'a t F.t -> 'a t

  let eta f =
    compose_right_to_left (fun a -> Bind a) (F.map (fun a -> Return a)) f
  ;;
end

module Free_functor (F : Preface_specs.Functor.CORE) = Functor.Via_map (struct
  include Preface_specs.Free.CORE

  let rec map f = function
    | Return v -> Return (f v)
    | Bind f' -> Bind (F.map (map f) f')
  ;;
end)

module Free_applicative (F : Preface_specs.Functor.CORE) =
Applicative.Via_apply (struct
  include Preface_specs.Free.CORE
  include Free_functor (F)

  let pure a = Return a

  let rec apply f a =
    match f with
    | Return f' -> map f' a
    | Bind f' -> Bind (F.map (fun f -> apply f a) f')
  ;;
end)

module Free_monad (F : Preface_specs.Functor.CORE) :
  Preface_specs.MONAD with type 'a t = 'a Preface_specs.Free.CORE.t =
Monad.Via_bind (struct
  include Preface_specs.Free.CORE
  include Free_functor (F)
  include Free_applicative (F)

  let return = pure

  let rec bind f = function
    | Return a -> f a
    | Bind a -> Bind (F.map (bind f) a)
  ;;
end)
