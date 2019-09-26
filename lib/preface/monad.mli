(** Modules for building [Monad] modules. *)

(** {1 Construction of a [Monad] module} *)

module Make_via_bind (Core_via_bind : Specs.Monad.CORE_VIA_BIND) :
  Specs.MONAD with type 'a t = 'a Core_via_bind.t

module Make_via_map_and_join
    (Core_via_map_and_join : Specs.Monad.CORE_VIA_MAP_AND_JOIN) :
  Specs.MONAD with type 'a t = 'a Core_via_map_and_join.t

module Make_via_kleisli_composition
    (Core_via_kleisli_composition : Specs.Monad.CORE_VIA_KLEISLI_COMPOSITION) :
  Specs.MONAD with type 'a t = 'a Core_via_kleisli_composition.t

module Make
    (Core : Specs.Monad.CORE)
    (Operation : Specs.Monad.OPERATION with type 'a t = 'a Core.t)
    (Infix : Specs.Monad.INFIX with type 'a t = 'a Core.t)
    (Syntax : Specs.Monad.SYNTAX with type 'a t = 'a Core.t) :
  Specs.MONAD with type 'a t = 'a Core.t

(** {1 Internal construction of a [Monad] module} *)

module Make_core_via_bind (Core : Specs.Monad.CORE_VIA_BIND) :
  Specs.Monad.CORE with type 'a t = 'a Core.t

module Make_core_via_map_and_join (Core : Specs.Monad.CORE_VIA_MAP_AND_JOIN) :
  Specs.Monad.CORE with type 'a t = 'a Core.t

module Make_core_via_kleisli_composition
    (Core : Specs.Monad.CORE_VIA_KLEISLI_COMPOSITION) :
  Specs.Monad.CORE with type 'a t = 'a Core.t

module Make_operation (Core : Specs.Monad.CORE) :
  Specs.Monad.OPERATION with type 'a t = 'a Core.t

module Make_syntax (Core : Specs.Monad.CORE) :
  Specs.Monad.SYNTAX with type 'a t = 'a Core.t

module Make_infix
    (Core : Specs.Monad.CORE)
    (Operation : Specs.Monad.OPERATION with type 'a t = 'a Core.t) :
  Specs.Monad.INFIX with type 'a t = 'a Core.t
