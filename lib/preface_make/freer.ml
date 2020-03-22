open Preface_core.Fun.Infix

module Via (F : sig
  type 'a data
end) : Preface_specs.FREER with type 'a data = 'a F.data = struct
  type 'a data = 'a F.data

  type 'a t =
    (* How to prevent this type definition here? *)
    | Return : 'a -> 'a t
    | Bind : 'b data * ('b -> 'a t) -> 'a t
end

module With_pure (CORE : Preface_specs.FREER) = struct
  include CORE

  let pure a = Return a
end

module With_map (CORE : Preface_specs.FREER) = struct
  include CORE

  let rec map f = function
    | Return x -> Return (f x)
    | Bind (i, c) -> Bind (i, c %> map f)
  ;;
end

(**  *)

module Via_functor (CORE : Preface_specs.FREER) :
  Preface_specs.FUNCTOR with type 'a t = 'a CORE.t = Functor.Via_map (struct
  include With_map (CORE)
end)

module Via_applicative (CORE : Preface_specs.FREER) :
  Preface_specs.APPLICATIVE with type 'a t = 'a CORE.t =
Applicative.Via_apply (struct
  include With_pure (CORE)
  include With_map (CORE)

  let rec apply f a =
    match f with
    | Return f' -> map f' a
    | Bind (i, c) -> Bind (i, c %> (fun f -> apply f a))
  ;;
end)

module Via_monad (CORE : Preface_specs.FREER) :
  Preface_specs.MONAD with type 'a t = 'a CORE.t = Monad.Via_bind (struct
  include With_pure (CORE)

  let return = pure

  let rec bind f = function
    | Return a -> f a
    | Bind (i, c) -> Bind (i, c %> bind f)
  ;;
end)
