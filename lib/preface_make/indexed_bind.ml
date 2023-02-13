open Preface_core.Fun

module Core_via_map_and_bind
    (Req : Preface_specs.Indexed_bind.WITH_MAP_AND_BIND) =
struct
  include Req

  let join m = bind id m
  let compose_left_to_right f g x = bind g (f x)
end

module Core_over_functor_via_bind
    (Functor : Preface_specs.Indexed_functor.WITH_MAP)
    (Req : Preface_specs.Indexed_bind.WITH_BIND
             with type ('a, 'index) t = ('a, 'index) Functor.t) =
struct
  include Core_via_map_and_bind (struct
    include Functor
    include Req
  end)
end

module Core_via_map_and_join
    (Req : Preface_specs.Indexed_bind.WITH_MAP_AND_JOIN) =
struct
  include Req

  let bind f m = join (map f m)
  let compose_left_to_right f g x = bind g (f x)
end

module Core_via_map_and_kleisli_composition
    (Req : Preface_specs.Indexed_bind.WITH_MAP_AND_KLEISLI_COMPOSITION) =
struct
  include Req

  let bind f m = (compose_left_to_right (const m) f) ()
  let join m = bind id m
end

module Core_over_functor_via_kleisli_composition
    (Functor : Preface_specs.Indexed_functor.WITH_MAP)
    (Req : Preface_specs.Indexed_bind.WITH_KLEISLI_COMPOSITION
             with type ('a, 'index) t = ('a, 'index) Functor.t) =
struct
  include Core_via_map_and_kleisli_composition (struct
    include Functor
    include Req
  end)
end

module Operation (Core : Preface_specs.Indexed_bind.CORE) = struct
  include Indexed_functor.Operation (Core)

  let compose_right_to_left f g x = Core.compose_left_to_right g f x
  let lift = Core.map
  let lift2 f ma mb = Core.(bind (fun a -> map (fun b -> f a b) mb) ma)

  let lift3 f ma mb mc =
    let open Core in
    bind (fun a -> bind (fun b -> map (fun c -> f a b c) mc) mb) ma
  ;;
end

module Syntax (Core : Preface_specs.Indexed_bind.CORE) = struct
  include Indexed_functor.Syntax (Core)

  let ( let* ) m f = Core.bind f m
end

module Infix
    (Core : Preface_specs.Indexed_bind.CORE)
    (Operation : Preface_specs.Indexed_bind.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t) =
struct
  include Indexed_functor.Infix (Core) (Operation)

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
    (Core : Preface_specs.Indexed_bind.CORE)
    (Operation : Preface_specs.Indexed_bind.OPERATION)
    (Infix : Preface_specs.Indexed_bind.INFIX)
    (Syntax : Preface_specs.Indexed_bind.SYNTAX) =
struct
  include Core
  include Operation
  include Syntax
  include Infix
  module Syntax = Syntax
  module Infix = Infix
end

module Via_map_and_bind (Req : Preface_specs.Indexed_bind.WITH_MAP_AND_BIND) =
struct
  module Core = Core_via_map_and_bind (Req)
  module Operation = Operation (Core)
  module Syntax = Syntax (Core)
  module Infix = Infix (Core) (Operation)
  include Core
  include Operation
  include Syntax
  include Infix
end

module Via_map_and_join (Req : Preface_specs.Indexed_bind.WITH_MAP_AND_JOIN) =
struct
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
    (Req : Preface_specs.Indexed_bind.WITH_MAP_AND_KLEISLI_COMPOSITION) =
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
    (Functor : Preface_specs.Indexed_functor.WITH_MAP)
    (Req : Preface_specs.Indexed_bind.WITH_BIND
             with type ('a, 'index) t = ('a, 'index) Functor.t) =
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
    (Functor : Preface_specs.Indexed_functor.WITH_MAP)
    (Req : Preface_specs.Indexed_bind.WITH_KLEISLI_COMPOSITION
             with type ('a, 'index) t = ('a, 'index) Functor.t) =
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
