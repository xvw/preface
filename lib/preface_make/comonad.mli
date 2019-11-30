(** Modules for building [Comonad] modules. *)

(** {1 Construction of a [Comonad] module} *)

module Via_map_and_duplicate
    (Core : Preface_specs.Comonad.CORE_VIA_MAP_AND_DUPLICATE) :
  Preface_specs.COMONAD with type 'a t = 'a Core.t

module Via_extend (Core : Preface_specs.Comonad.CORE_VIA_EXTEND) :
  Preface_specs.COMONAD with type 'a t = 'a Core.t

module Via_cokleisli_composition
    (Core : Preface_specs.Comonad.CORE_VIA_COKLEISLI_COMPOSITION) :
  Preface_specs.COMONAD with type 'a t = 'a Core.t

module Make
    (Core : Preface_specs.Comonad.CORE)
    (Operation : Preface_specs.Comonad.OPERATION with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Comonad.SYNTAX with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Comonad.INFIX with type 'a t = 'a Core.t) :
  Preface_specs.COMONAD with type 'a t = 'a Core.t

(** {1 Internal construction of a [Comonad] module} *)

module Core_via_map_and_duplicate
    (Core : Preface_specs.Comonad.CORE_VIA_MAP_AND_DUPLICATE) :
  Preface_specs.Comonad.CORE with type 'a t = 'a Core.t

module Core_via_extend (Core : Preface_specs.Comonad.CORE_VIA_EXTEND) :
  Preface_specs.Comonad.CORE with type 'a t = 'a Core.t

module Core_via_cokleisli_composition
    (Core : Preface_specs.Comonad.CORE_VIA_COKLEISLI_COMPOSITION) :
  Preface_specs.Comonad.CORE with type 'a t = 'a Core.t

module Operation (Core : Preface_specs.Comonad.CORE) :
  Preface_specs.Comonad.OPERATION with type 'a t = 'a Core.t

module Infix
    (Core : Preface_specs.Comonad.CORE)
    (Operation : Preface_specs.Comonad.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Comonad.INFIX with type 'a t = 'a Core.t
