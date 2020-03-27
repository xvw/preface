open Preface_core.Fun.Infix

module Via (F : sig
  type 'a g
end) =
struct
  type 'a g = 'a F.g

  type 'a t =
    | Return : 'a -> 'a t
    | Bind : 'b g * ('b -> 'a t) -> 'a t

  let lift f = Bind (f, (fun a -> Return a))
end

module With_pure (Freer : Preface_specs.FREER) = struct
  type 'a t = 'a Freer.t

  let pure a = Freer.(Return a)
end

module With_map (Freer : Preface_specs.FREER) = struct
  include Freer

  let rec map f =
    let open Freer in
    (function Return x -> Return (f x) | Bind (i, c) -> Bind (i, c %> map f))
  ;;
end

(**  *)

module Via_functor (Freer : Preface_specs.FREER) :
  Preface_specs.FUNCTOR with type 'a t = 'a Freer.t = Functor.Via_map (struct
  include With_map (Freer)
end)

module Via_applicative (Freer : Preface_specs.FREER) :
  Preface_specs.APPLICATIVE with type 'a t = 'a Freer.t =
Applicative.Via_apply (struct
  include With_pure (Freer)
  include With_map (Freer)

  let rec apply f a =
    let open Freer in
    match f with
    | Return f' -> map f' a
    | Bind (i, c) -> Bind (i, c %> (fun f -> apply f a))
  ;;
end)

module Via_monad (Freer : Preface_specs.FREER) :
  Preface_specs.MONAD with type 'a t = 'a Freer.t = Monad.Via_bind (struct
  include With_pure (Freer)

  let return = pure

  let rec bind f =
    Freer.((function Return a -> f a | Bind (i, c) -> Bind (i, c %> bind f)))
  ;;
end)
