open Fun.Infix

module Make_freeer_functor (F : sig type 'a t end) : Specs.Functor.CORE = struct
  include Specs.Freeer.CORE_with_type (F)

  let rec map f = function
    | Return x -> Return (f x)
    | FlatMap (i, c) -> FlatMap (i, c %> (map f))
end
