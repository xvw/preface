open Aliases

module type LAWS = sig
  include Semigroupoid.LAWS

  val right_identity : string * (('a, 'b) t -> ('a, 'b) t pair)
  val left_identity : string * (('a, 'b) t -> ('a, 'b) t pair)
end

module Laws (C : Preface_specs.CATEGORY) = struct
  include Semigroupoid.Laws (C)

  let right_identity = ("f % id = f", fun f -> (C.(f % id), f))
  let left_identity = ("id % f = f", fun f -> (C.(id % f), f))
end
