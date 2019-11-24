module Make_free_functor (F : Specs.Functor.CORE) : Specs.Functor.CORE = struct
  include Specs.Free.CORE_with_type (F)

  let rec map f = function
    | Return v -> Return (f v)
    | FlatMap f' -> FlatMap (F.map (map f) f')
end

module Make_free_applicative (F : Specs.Functor.CORE) : Specs.Applicative.CORE_VIA_APPLY = struct
  include Specs.Free.CORE_with_type (F)

  let pure a = Return a

  let rec map f = function
    | Return v -> Return (f v)
    | FlatMap f' -> FlatMap (F.map (map f) f')

  let rec apply f a =
    match f with
    | Return f' -> map f' a
    | FlatMap f' -> FlatMap (F.map (fun f -> apply f a) f')

end
