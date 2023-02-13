open Preface_core.Fun

module Core_via_return_and_bind
    (Req : Preface_specs.Indexed_monad.WITH_RETURN_AND_BIND) =
struct
  include Req

  let join m = bind id m
  let map f m = bind (return <% f) m
  let compose_left_to_right f g x = bind g (f x)
end

module Core_via_return_map_and_join
    (Req : Preface_specs.Indexed_monad.WITH_RETURN_MAP_AND_JOIN) =
struct
  include Req

  let bind f m = join (map f m)
  let compose_left_to_right f g x = bind g (f x)
end

module Core_via_return_and_kleisli_composition
    (Req : Preface_specs.Indexed_monad.WITH_RETURN_AND_KLEISLI_COMPOSITION) =
struct
  include Req

  let bind f m = (compose_left_to_right (const m) f) ()
  let join m = bind id m
  let map f m = bind (return <% f) m
end

module Operation (Core : Preface_specs.Indexed_monad.CORE) = struct
  include Indexed_functor.Operation (Core)

  let void _ = Core.return ()
  let compose_right_to_left f g x = Core.compose_left_to_right g f x
  let lift = Core.map
  let lift2 f ma mb = Core.(bind (fun a -> bind (fun b -> return (f a b)) mb) ma)

  let lift3 f ma mb mc =
    let open Core in
    bind (fun a -> bind (fun b -> bind (fun c -> return (f a b c)) mc) mb) ma
  ;;
end

module Syntax (Core : Preface_specs.Indexed_bind.CORE) = struct
  type ('a, 'index) t = ('a, 'index) Core.t

  let ( let* ) m f = Core.bind f m
  let ( let+ ) m f = Core.map f m
end

module Infix
    (Core : Preface_specs.Indexed_monad.CORE)
    (Operation : Preface_specs.Indexed_monad.OPERATION
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
    (Core : Preface_specs.Indexed_monad.CORE)
    (Operation : Preface_specs.Indexed_monad.OPERATION)
    (Infix : Preface_specs.Indexed_monad.INFIX)
    (Syntax : Preface_specs.Indexed_monad.SYNTAX) =
struct
  include Core
  include Operation
  include Syntax
  include Infix
  module Syntax = Syntax
  module Infix = Infix
end

module Via_return_and_bind
    (Req : Preface_specs.Indexed_monad.WITH_RETURN_AND_BIND) =
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
    (Req : Preface_specs.Indexed_monad.WITH_RETURN_MAP_AND_JOIN) =
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
    (Req : Preface_specs.Indexed_monad.WITH_RETURN_AND_KLEISLI_COMPOSITION) =
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
