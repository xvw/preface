module Make_free_functor (F : Specs.Functor.CORE) : Specs.Functor.CORE = struct
  include Specs.Free.CORE_with_type (F)

  let rec map f = function
    | Return v -> Return (f v)
    | FlatMap f' -> FlatMap (F.map (map f) f')
end

module Make_free_applicative (F : Specs.Applicative.CORE) : Specs.Functor.CORE =
struct
  include Specs.Free.CORE_with_type (F)

  let rec map f = function
    | Return v -> Return (f v)
    | FlatMap f' -> FlatMap (F.map (map f) f')
end
