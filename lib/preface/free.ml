module Make_free_functor (F : Specs.Functor.CORE) :
  Specs.Functor.CORE with type 'a t = 'a Specs.Free.CORE(F).t = struct
  include Specs.Free.CORE (F)

  let rec map f = function
    | Return v -> Return (f v)
    | FlatMap f' -> FlatMap (F.map (map f) f')
end

module Make_free_applicative (F : Specs.Functor.CORE) :
  Specs.Applicative.CORE with type 'a t = 'a Specs.Free.CORE(F).t =
Applicative.Make_core_via_apply (struct
  include Specs.Free.CORE (F)
  include Make_free_functor (F)

  let pure a = Return a

  let rec apply f a =
    match f with
    | Return f' -> map f' a
    | FlatMap f' -> FlatMap (F.map (fun f -> apply f a) f')
end)

module Make_free_monad (F : Specs.Functor.CORE) :
  Specs.Monad.CORE with type 'a t = 'a Specs.Free.CORE(F).t =
Monad.Make_core_via_bind (struct
  include Specs.Free.CORE (F)
  include Make_free_functor (F)
  include Make_free_applicative (F)

  let return = pure

  let rec bind f = function
    | Return a -> f a
    | FlatMap a -> FlatMap (F.map (bind f) a)
end)
