(** Modules for building [Applicative] modules. *)

(** {1 Construction of an [Applicative] module} *)

module Make_via_map_and_product
    (Core_via_map_and_product : Preface_specs.Applicative
                                .CORE_VIA_MAP_AND_PRODUCT) :
  Preface_specs.APPLICATIVE with type 'a t = 'a Core_via_map_and_product.t
(** Incarnation of an [Applicative] for an ['a t] with standard
    Requirements ([pure], [map] and [product]).
*)

module Make_via_apply
    (Core_via_apply : Preface_specs.Applicative.CORE_VIA_APPLY) :
  Preface_specs.APPLICATIVE with type 'a t = 'a Core_via_apply.t
(** Incarnation of an [Applicative] for an ['a t] with standard
    Requirements ([pure] and [apply]).
*)

module Make_via_monad (Monad : Preface_specs.MONAD) :
  Preface_specs.APPLICATIVE with type 'a t = 'a Monad.t
(** Incarnation of an [Applicative] using a [Monad].*)

module Make
    (Core : Preface_specs.Applicative.CORE)
    (Operation : Preface_specs.Applicative.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Applicative.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Applicative.SYNTAX with type 'a t = 'a Core.t) :
  Preface_specs.APPLICATIVE with type 'a t = 'a Core.t
(** Incarnation of an [Applicative] for an ['a t] using each components of 
    an [Applicative].
*)

(** {1 Internal construction of an [Applicative] module} *)

module Make_core_via_map_and_product
    (Core : Preface_specs.Applicative.CORE_VIA_MAP_AND_PRODUCT) :
  Preface_specs.Applicative.CORE with type 'a t = 'a Core.t
(** Incarnation of an [Applicative.Core] for an ['a t] with standard
    Requirements ([pure], [map] and [product]).
*)

module Make_core_via_apply (Core : Preface_specs.Applicative.CORE_VIA_APPLY) :
  Preface_specs.Applicative.CORE with type 'a t = 'a Core.t
(** Incarnation of an [Applicative.Core] for an ['a t] with standard
    Requirements ([pure], [apply]).
*)

module Make_operation (Core : Preface_specs.Applicative.CORE) :
  Preface_specs.Applicative.OPERATION with type 'a t = 'a Core.t
(** Incarnation of an [Applicative.Operation] for an ['a t] with standard
    Requirements ([pure], [map], [apply] and [product]).
*)

module Make_syntax (Core : Preface_specs.Applicative.CORE) :
  Preface_specs.Applicative.SYNTAX with type 'a t = 'a Core.t
(** Incarnation of an [Applicative.Syntax] for an ['a t] with standard
    Requirements ([pure], [map], [apply] and [product]).
*)

module Make_infix
    (Core : Preface_specs.Applicative.CORE)
    (Operation : Preface_specs.Applicative.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Applicative.INFIX with type 'a t = 'a Core.t
(** Incarnation of an [Applicative.Infix] for an ['a t] with standard
    Requirements ([pure], [map], [apply] and [product]).
*)
