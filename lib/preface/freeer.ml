open Fun.Infix

module Make_freeer_functor (F : sig
  type 'a t
end) : Specs.Functor.CORE with type 'a t = 'a Specs.Freeer.CORE(F).t = struct
  include Specs.Freeer.CORE (F)

  let rec map f = function
    | Return x -> Return (f x)
    | FlatMap (i, c) -> FlatMap (i, c %> map f)
end
