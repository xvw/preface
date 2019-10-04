(** Modules for building [Monad] modules. *)

(** {1 Construction of a [Monad] module} *)

module Make_via_bind (Core_via_bind : Preface_specs.Monad.CORE_VIA_BIND) :
  Preface_specs.MONAD with type 'a t = 'a Core_via_bind.t

module Make_via_map_and_join
    (Core_via_map_and_join : Preface_specs.Monad.CORE_VIA_MAP_AND_JOIN) :
  Preface_specs.MONAD with type 'a t = 'a Core_via_map_and_join.t

module Make_via_kleisli_composition
    (Core_via_kleisli_composition : Preface_specs.Monad
                                    .CORE_VIA_KLEISLI_COMPOSITION) :
  Preface_specs.MONAD with type 'a t = 'a Core_via_kleisli_composition.t

module Make
    (Core : Preface_specs.Monad.CORE)
    (Operation : Preface_specs.Monad.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Monad.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Monad.SYNTAX with type 'a t = 'a Core.t) :
  Preface_specs.MONAD with type 'a t = 'a Core.t

(** {1 Internal construction of a [Monad] module} *)

module Make_core_via_bind (Core : Preface_specs.Monad.CORE_VIA_BIND) :
  Preface_specs.Monad.CORE with type 'a t = 'a Core.t

module Make_core_via_map_and_join
    (Core : Preface_specs.Monad.CORE_VIA_MAP_AND_JOIN) :
  Preface_specs.Monad.CORE with type 'a t = 'a Core.t

module Make_core_via_kleisli_composition
    (Core : Preface_specs.Monad.CORE_VIA_KLEISLI_COMPOSITION) :
  Preface_specs.Monad.CORE with type 'a t = 'a Core.t

module Make_operation (Core : Preface_specs.Monad.CORE) :
  Preface_specs.Monad.OPERATION with type 'a t = 'a Core.t

module Make_syntax (Core : Preface_specs.Monad.CORE) :
  Preface_specs.Monad.SYNTAX with type 'a t = 'a Core.t

module Make_infix
    (Core : Preface_specs.Monad.CORE)
    (Operation : Preface_specs.Monad.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Monad.INFIX with type 'a t = 'a Core.t
