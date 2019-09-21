open Fun

module Operation (Core : Specs.Applicative.Core) :
  Specs.Applicative.Operation with type 'a t = 'a Core.t = struct
  include Core
  include Functor

  let ap _f _a = failwith "Map not yet 'included'" (* WIP *)

  let liftA f = ap @@ pure f

  let liftA2 f a = ap @@ ap (pure f) a

  let liftA3 f a b = ap @@ ap (ap (pure f) a) b
end

module Syntax (Core : Specs.Applicative.Core) :
  Specs.Applicative.Syntax with type 'a t = 'a Core.t = struct
  include Core
  include Operation (Core)

  let ( let+ ) _x _f = failwith "Map not yet 'included'" (* WIP *)

  let ( and+ ) = product
end

module Infix (Core : Specs.Applicative.Core) :
  Specs.Applicative.Infix with type 'a t = 'a Core.t = struct
  include Core
  include Operation (Core)

  let ( <*> ) = ap

  let ( <**> ) a f = f <*> a

  let ( <* ) a b = liftA2 const a b

  let ( *> ) a b = b <* a
end
