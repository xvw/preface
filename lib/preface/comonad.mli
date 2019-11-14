(** Modules for building [Comonad] modules. *)

(** {1 Construction of a [Comonad] module} *)

module Make_via_map_and_duplicate
    (Core : Specs.Comonad.CORE_VIA_MAP_AND_DUPLICATE) :
  Specs.COMONAD with type 'a t = 'a Core.t

module Make_via_extend (Core : Specs.Comonad.CORE_VIA_EXTEND) :
  Specs.COMONAD with type 'a t = 'a Core.t

module Make_via_cokleisli_composition
    (Core : Specs.Comonad.CORE_VIA_COKLEISLI_COMPOSITION) :
  Specs.COMONAD with type 'a t = 'a Core.t

module Make
    (Core : Specs.Comonad.CORE)
    (Operation : Specs.Comonad.OPERATION with type 'a t = 'a Core.t)
    (Infix : Specs.Comonad.INFIX with type 'a t = 'a Core.t) :
  Specs.COMONAD with type 'a t = 'a Core.t

(** {1 Internal construction of a [Comonad] module} *)

module Make_core_via_map_and_duplicate
    (Core : Specs.Comonad.CORE_VIA_MAP_AND_DUPLICATE) :
  Specs.Comonad.CORE with type 'a t = 'a Core.t

module Make_core_via_extend (Core : Specs.Comonad.CORE_VIA_EXTEND) :
  Specs.Comonad.CORE with type 'a t = 'a Core.t

module Make_core_via_cokleisli_composition
    (Core : Specs.Comonad.CORE_VIA_COKLEISLI_COMPOSITION) :
  Specs.Comonad.CORE with type 'a t = 'a Core.t

module Make_operation (Core : Specs.Comonad.CORE) :
  Specs.Comonad.OPERATION with type 'a t = 'a Core.t

module Make_infix
    (Core : Specs.Comonad.CORE)
    (Operation : Specs.Comonad.OPERATION with type 'a t = 'a Core.t) :
  Specs.Comonad.INFIX with type 'a t = 'a Core.t
