open Preface_core.Fun.Infix

module FreeEr_functor (F : sig
  type 'a t
end) : Preface_specs.FUNCTOR with type 'a t = 'a Preface_specs.FreeEr.CORE(F).t =
Functor.Via_map(struct
  include Preface_specs.FreeEr.CORE (F)

  let rec map f = function
    | Return x -> Return (f x)
    | Bind (i, c) -> Bind (i, c %> map f)
end)

module FreeEr_applicative (F : sig
  type 'a t
end) :
  Preface_specs.APPLICATIVE with type 'a t = 'a Preface_specs.FreeEr.CORE(F).t =
Applicative.Via_apply (struct
  include Preface_specs.FreeEr.CORE (F)
  include FreeEr_functor (F)

  let pure a = Return a

  let rec apply f a =
    match f with
    | Return f' -> map f' a
    | Bind (i, c) -> Bind (i, c %> (fun f -> apply f a))
end)

module FreeEr_monad (F : sig
  type 'a t
end) :
  Preface_specs.MONAD with type 'a t = 'a Preface_specs.FreeEr.CORE(F).t =
Monad.Via_bind (struct
  include Preface_specs.FreeEr.CORE (F)
  include FreeEr_functor (F)
  include FreeEr_applicative (F)

  let return = pure

  let rec bind f = function
    | Return a -> f a
    | Bind (i, c) -> Bind (i, c %> bind f)
end)