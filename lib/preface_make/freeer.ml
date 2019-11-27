open Fun.Infix

module Make_freeer_functor (F : sig
  type 'a t
end) : Specs.FUNCTOR with type 'a t = 'a Specs.Freeer.CORE(F).t =
Functor.Make_via_map(struct
  include Specs.Freeer.CORE (F)

  let rec map f = function
    | Return x -> Return (f x)
    | FlatMap (i, c) -> FlatMap (i, c %> map f)
end)

module Make_freeer_applicative (F : sig
  type 'a t
end) :
  Specs.APPLICATIVE with type 'a t = 'a Specs.Freeer.CORE(F).t =
Applicative.Make_via_apply (struct
  include Specs.Freeer.CORE (F)
  include Make_freeer_functor (F)

  let pure a = Return a

  let rec apply f a =
    match f with
    | Return f' -> map f' a
    | FlatMap (i, c) -> FlatMap (i, c %> (fun f -> apply f a))
end)

module Make_freeer_monad (F : sig
  type 'a t
end) :
  Specs.MONAD with type 'a t = 'a Specs.Freeer.CORE(F).t =
Monad.Make_via_bind (struct
  include Specs.Freeer.CORE (F)
  include Make_freeer_functor (F)
  include Make_freeer_applicative (F)

  let return = pure

  let rec bind f = function
    | Return a -> f a
    | FlatMap (i, c) -> FlatMap (i, c %> bind f)
end)