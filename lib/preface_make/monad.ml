open Preface_core.Fun

module Core_via_return_and_bind (Req : Preface_specs.Monad.WITH_RETURN_AND_BIND) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_monad.Core_via_return_and_bind (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req :
          Preface_specs.Monad.WITH_RETURN_AND_BIND with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_monad.CORE with type ('a, _) t := 'a Req.t )
end

module Core_via_return_map_and_join
    (Req : Preface_specs.Monad.WITH_RETURN_MAP_AND_JOIN) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_monad.Core_via_return_map_and_join (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req :
          Preface_specs.Monad.WITH_RETURN_MAP_AND_JOIN
            with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_monad.CORE with type ('a, _) t := 'a Req.t )
end

module Core_via_return_and_kleisli_composition
    (Req : Preface_specs.Monad.WITH_RETURN_AND_KLEISLI_COMPOSITION) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_monad.Core_via_return_and_kleisli_composition (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req :
          Preface_specs.Monad.WITH_RETURN_AND_KLEISLI_COMPOSITION
            with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_monad.CORE with type ('a, _) t := 'a Req.t )
end

module Operation (Core : Preface_specs.Monad.CORE) = struct
  type 'a t = 'a Core.t

  include (
    Indexed_monad.Operation (struct
      type ('a, 'index) t = 'a Core.t

      include (Core : Preface_specs.Monad.CORE with type 'a t := 'a Core.t)
    end) :
      Preface_specs.Indexed_monad.OPERATION with type ('a, _) t := 'a Core.t )
end

module Syntax (Core : Preface_specs.Monad.CORE) = struct
  type 'a t = 'a Core.t

  include (
    Indexed_monad.Syntax (struct
      type ('a, 'index) t = 'a Core.t

      include (Core : Preface_specs.Monad.CORE with type 'a t := 'a Core.t)
    end) :
      Preface_specs.Indexed_monad.SYNTAX with type ('a, _) t := 'a Core.t )
end

module Infix
    (Core : Preface_specs.Monad.CORE)
    (Operation : Preface_specs.Monad.OPERATION with type 'a t = 'a Core.t) =
struct
  type 'a t = 'a Core.t

  include (
    Indexed_monad.Infix
      (struct
        type ('a, 'index) t = 'a Core.t

        include (Core : Preface_specs.Monad.CORE with type 'a t := 'a Core.t)
      end)
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Operation :
            Preface_specs.Monad.OPERATION with type 'a t := 'a Operation.t )
      end) :
      Preface_specs.Indexed_monad.INFIX with type ('a, _) t := 'a Core.t )
end

module Via
    (Core : Preface_specs.Monad.CORE)
    (Operation : Preface_specs.Monad.OPERATION)
    (Infix : Preface_specs.Monad.INFIX)
    (Syntax : Preface_specs.Monad.SYNTAX) =
struct
  include Core
  include Operation
  include Syntax
  include Infix
  module Syntax = Syntax
  module Infix = Infix
end

module Via_return_and_bind (Req : Preface_specs.Monad.WITH_RETURN_AND_BIND) =
struct
  module Core = Core_via_return_and_bind (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_return_map_and_join
    (Req : Preface_specs.Monad.WITH_RETURN_MAP_AND_JOIN) =
struct
  module Core = Core_via_return_map_and_join (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_return_and_kleisli_composition
    (Req : Preface_specs.Monad.WITH_RETURN_AND_KLEISLI_COMPOSITION) =
struct
  module Core = Core_via_return_and_kleisli_composition (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module From_monad_plus (Monad_plus : Preface_specs.MONAD_PLUS) = Monad_plus

module From_arrow_apply (A : Preface_specs.ARROW_APPLY) =
Via_return_and_bind (struct
  type 'a t = (unit, 'a) A.t

  let return x = A.arrow (const x)
  let bind f x = A.(x >>> arrow (fun x -> (f x, ())) >>> apply)
end)

module Product (F : Preface_specs.MONAD) (G : Preface_specs.MONAD) =
Via_return_and_bind (struct
  type 'a t = 'a F.t * 'a G.t

  let return x = (F.return x, G.return x)
  let bind f (m, n) = (F.bind (fst % f) m, G.bind (snd % f) n)
end)

module Index (F : Preface_specs.MONAD) = struct
  type ('a, 'index) t = 'a F.t

  include (
    Indexed_monad.Via
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F : Preface_specs.Monad.CORE with type 'a t := 'a F.t)
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F : Preface_specs.Monad.OPERATION with type 'a t := 'a F.t)
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F.Infix : Preface_specs.Monad.INFIX with type 'a t := 'a F.t)
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F.Syntax : Preface_specs.Monad.SYNTAX with type 'a t := 'a F.t)
      end) :
      Preface_specs.INDEXED_MONAD with type ('a, 'index) t := ('a, 'index) t )
end
