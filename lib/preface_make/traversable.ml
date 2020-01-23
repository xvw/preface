open Preface_core.Fun

module Core_via_applicative
    (A : Preface_specs.APPLICATIVE)
    (C : Preface_specs.Traversable.CORE with type 'a t = 'a A.t) :
  Preface_specs.Traversable.CORE
    with type 'a t = 'a C.t
     and type 'a iter = 'a C.iter = struct
  type 'a t = 'a C.t

  type 'a iter = 'a C.iter

  let traverse f x = C.traverse f x
end

module Core_via_monad
    (M : Preface_specs.MONAD)
    (C : Preface_specs.Traversable.CORE with type 'a t = 'a M.t) :
  Preface_specs.Traversable.CORE
    with type 'a t = 'a C.t
     and type 'a iter = 'a C.iter = struct
  type 'a t = 'a C.t

  type 'a iter = 'a C.iter

  let traverse f x = C.traverse f x
end

module Operation (C : Preface_specs.Traversable.CORE) :
  Preface_specs.Traversable.OPERATION
    with type 'a t = 'a C.t
     and type 'a iter = 'a C.iter = struct
  type 'a t = 'a C.t

  type 'a iter = 'a C.iter

  let sequence x = C.traverse id x
end

module Via
    (C : Preface_specs.Traversable.CORE)
    (O : Preface_specs.Traversable.OPERATION
           with type 'a t = 'a C.t
            and type 'a iter = 'a C.iter) :
  Preface_specs.TRAVERSABLE with type 'a t = 'a C.t and type 'a iter = 'a C.iter =
struct
  include C
  include O
end

module Via_applicative
    (A : Preface_specs.APPLICATIVE)
    (C : Preface_specs.Traversable.CORE with type 'a t = 'a A.t) :
  Preface_specs.TRAVERSABLE with type 'a t = 'a C.t and type 'a iter = 'a C.iter =
struct
  module Core = Core_via_applicative (A) (C)
  module Operation = Operation (Core)
  include Core
  include Operation
end

module Via_monad
    (M : Preface_specs.MONAD)
    (C : Preface_specs.Traversable.CORE with type 'a t = 'a M.t) :
  Preface_specs.TRAVERSABLE with type 'a t = 'a C.t and type 'a iter = 'a C.iter =
struct
  module Core = Core_via_monad (M) (C)
  module Operation = Operation (Core)
  include Core
  include Operation
end
