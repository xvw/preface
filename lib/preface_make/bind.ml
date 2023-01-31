module Core_via_map_and_bind (Req : Preface_specs.Bind.WITH_MAP_AND_BIND) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_bind.Core_via_map_and_bind (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req : Preface_specs.Bind.WITH_MAP_AND_BIND with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_bind.CORE with type ('a, _) t := 'a Req.t )
end

module Core_over_functor_via_bind
    (F : Preface_specs.Functor.WITH_MAP)
    (Req : Preface_specs.Bind.WITH_BIND with type 'a t = 'a F.t) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_bind.Core_over_functor_via_bind
      (struct
        type ('a, 'index) t = 'a F.t

        include (F : Preface_specs.Functor.WITH_MAP with type 'a t := 'a Req.t)
      end)
      (struct
        type ('a, 'index) t = 'a Req.t

        include (Req : Preface_specs.Bind.WITH_BIND with type 'a t := 'a Req.t)
      end) :
      Preface_specs.Indexed_bind.CORE with type ('a, _) t := 'a Req.t )
end

module Core_via_map_and_join (Req : Preface_specs.Bind.WITH_MAP_AND_JOIN) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_bind.Core_via_map_and_join (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req : Preface_specs.Bind.WITH_MAP_AND_JOIN with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_bind.CORE with type ('a, _) t := 'a Req.t )
end

module Core_via_map_and_kleisli_composition
    (Req : Preface_specs.Bind.WITH_MAP_AND_KLEISLI_COMPOSITION) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_bind.Core_via_map_and_kleisli_composition (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req :
          Preface_specs.Bind.WITH_MAP_AND_KLEISLI_COMPOSITION
            with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_bind.CORE with type ('a, _) t := 'a Req.t )
end

module Core_over_functor_via_kleisli_composition
    (F : Preface_specs.Functor.WITH_MAP)
    (Req : Preface_specs.Bind.WITH_KLEISLI_COMPOSITION with type 'a t = 'a F.t) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_bind.Core_over_functor_via_kleisli_composition
      (struct
        type ('a, 'index) t = 'a F.t

        include (F : Preface_specs.Functor.WITH_MAP with type 'a t := 'a Req.t)
      end)
      (struct
        type ('a, 'index) t = 'a Req.t

        include (
          Req :
            Preface_specs.Bind.WITH_KLEISLI_COMPOSITION
              with type 'a t := 'a Req.t )
      end) :
      Preface_specs.Indexed_bind.CORE with type ('a, _) t := 'a Req.t )
end

module Operation (Core : Preface_specs.Bind.CORE) = struct
  type 'a t = 'a Core.t

  include (
    Indexed_bind.Operation (struct
      type ('a, 'index) t = 'a Core.t

      include (Core : Preface_specs.Bind.CORE with type 'a t := 'a Core.t)
    end) :
      Preface_specs.Indexed_bind.OPERATION with type ('a, _) t := 'a Core.t )
end

module Syntax (Core : Preface_specs.Bind.CORE) = struct
  type 'a t = 'a Core.t

  include (
    Indexed_bind.Syntax (struct
      type ('a, 'index) t = 'a Core.t

      include (Core : Preface_specs.Bind.CORE with type 'a t := 'a Core.t)
    end) :
      Preface_specs.Indexed_bind.SYNTAX with type ('a, _) t := 'a Core.t )
end

module Infix
    (Core : Preface_specs.Bind.CORE)
    (Operation : Preface_specs.Bind.OPERATION with type 'a t = 'a Core.t) =
struct
  type 'a t = 'a Core.t

  include (
    Indexed_bind.Infix
      (struct
        type ('a, 'index) t = 'a Core.t

        include (Core : Preface_specs.Bind.CORE with type 'a t := 'a Core.t)
      end)
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Operation :
            Preface_specs.Bind.OPERATION with type 'a t := 'a Operation.t )
      end) :
      Preface_specs.Indexed_bind.INFIX with type ('a, _) t := 'a Core.t )
end

module Via
    (Core : Preface_specs.Bind.CORE)
    (Operation : Preface_specs.Bind.OPERATION)
    (Infix : Preface_specs.Bind.INFIX)
    (Syntax : Preface_specs.Bind.SYNTAX) =
struct
  include Core
  include Operation
  include Syntax
  include Infix
  module Syntax = Syntax
  module Infix = Infix
end

module Via_map_and_bind (Req : Preface_specs.Bind.WITH_MAP_AND_BIND) = struct
  module Core = Core_via_map_and_bind (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_map_and_join (Req : Preface_specs.Bind.WITH_MAP_AND_JOIN) = struct
  module Core = Core_via_map_and_join (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_map_and_kleisli_composition
    (Req : Preface_specs.Bind.WITH_MAP_AND_KLEISLI_COMPOSITION) =
struct
  module Core = Core_via_map_and_kleisli_composition (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Over_functor_via_bind
    (Functor : Preface_specs.Functor.WITH_MAP)
    (Req : Preface_specs.Bind.WITH_BIND with type 'a t = 'a Functor.t) =
struct
  module Core = Core_over_functor_via_bind (Functor) (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Over_functor_via_kleisli_composition
    (Functor : Preface_specs.Functor.WITH_MAP)
    (Req : Preface_specs.Bind.WITH_KLEISLI_COMPOSITION
             with type 'a t = 'a Functor.t) =
struct
  module Core = Core_over_functor_via_kleisli_composition (Functor) (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module From_monad (Monad : Preface_specs.MONAD) = Monad
module From_monad_plus (Monad_plus : Preface_specs.MONAD_PLUS) = Monad_plus

module From_arrow_apply (A : Preface_specs.ARROW_APPLY) =
  Over_functor_via_bind
    (struct
      type 'a t = (unit, 'a) A.t

      let map f x = A.(x >>> arrow f)
    end)
    (struct
      type 'a t = (unit, 'a) A.t

      let bind f x = A.(x >>> arrow (fun x -> (f x, ())) >>> apply)
    end)

module Product (F : Preface_specs.BIND) (G : Preface_specs.BIND) =
  Over_functor_via_bind
    (struct
      type 'a t = 'a F.t * 'a G.t

      let map f (x, y) = (F.map f x, G.map f y)
    end)
    (struct
      type 'a t = 'a F.t * 'a G.t

      let bind f (m, n) =
        let open Preface_core.Fun.Infix in
        (F.bind (fst % f) m, G.bind (snd % f) n)
      ;;
    end)

module Index (F : Preface_specs.BIND) = struct
  type ('a, 'index) t = 'a F.t

  include (
    Indexed_bind.Via
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F : Preface_specs.Bind.CORE with type 'a t := 'a F.t)
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F : Preface_specs.Bind.OPERATION with type 'a t := 'a F.t)
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F.Infix : Preface_specs.Bind.INFIX with type 'a t := 'a F.t)
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F.Syntax : Preface_specs.Bind.SYNTAX with type 'a t := 'a F.t)
      end) :
      Preface_specs.INDEXED_BIND with type ('a, 'index) t := ('a, 'index) t )
end
