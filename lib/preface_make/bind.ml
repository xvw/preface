open Preface_core.Fun

module Core_via_map_and_bind (Req : Preface_specs.Bind.WITH_MAP_AND_BIND) =
struct
  include Req

  let join m = bind id m
  let compose_left_to_right f g x = bind g (f x)
end

module Core_over_functor_via_bind
    (Functor : Preface_specs.Functor.WITH_MAP)
    (Req : Preface_specs.Bind.WITH_BIND with type 'a t = 'a Functor.t) =
struct
  include Core_via_map_and_bind (struct
    include Functor
    include Req
  end)
end

module Core_via_map_and_join (Req : Preface_specs.Bind.WITH_MAP_AND_JOIN) =
struct
  include Req

  let bind f m = join (map f m)
  let compose_left_to_right f g x = bind g (f x)
end

module Core_via_map_and_kleisli_composition
    (Req : Preface_specs.Bind.WITH_MAP_AND_KLEISLI_COMPOSITION) =
struct
  include Req

  let bind f m = (compose_left_to_right (const m) f) ()
  let join m = bind id m
end

module Core_over_functor_via_kleisli_composition
    (Functor : Preface_specs.Functor.WITH_MAP)
    (Req : Preface_specs.Bind.WITH_KLEISLI_COMPOSITION
             with type 'a t = 'a Functor.t) =
struct
  include Core_via_map_and_kleisli_composition (struct
    include Functor
    include Req
  end)
end

module Operation (Core : Preface_specs.Bind.CORE) = struct
  include Functor.Operation (Core)

  let compose_right_to_left f g x = Core.compose_left_to_right g f x
  let lift = Core.map
  let lift2 f ma mb = Core.(bind (fun a -> map (fun b -> f a b) mb) ma)

  let lift3 f ma mb mc =
    let open Core in
    bind (fun a -> bind (fun b -> map (fun c -> f a b c) mc) mb) ma
  ;;
end

module Syntax (Core : Preface_specs.Bind.CORE) = struct
  type 'a t = 'a Core.t

  let ( let* ) m f = Core.bind f m
  let ( let+ ) m f = Core.map f m
end

module Infix
    (Core : Preface_specs.Bind.CORE)
    (Operation : Preface_specs.Bind.OPERATION with type 'a t = 'a Core.t) =
struct
  include Functor.Infix (Core) (Operation)

  let ( >|= ) x f = Core.map f x
  let ( =|< ) = Core.map
  let ( >>= ) x f = Core.bind f x
  let ( =<< ) = Core.bind
  let ( >=> ) = Core.compose_left_to_right
  let ( <=< ) = Operation.compose_right_to_left
  let ( >> ) ma mb = ma >>= fun _ -> mb
  let ( << ) ma _ = ma
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

module From_monad (Monad : Preface_specs.MONAD)  = Monad

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

      let bind f (m, n) = (F.bind (fst % f) m, G.bind (snd % f) n)
    end)
