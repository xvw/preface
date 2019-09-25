module Make_via_map_and_product
    (Core_via_map_and_product : Specs.Applicative.CORE_VIA_MAP_AND_PRODUCT) :
  Specs.APPLICATIVE with type 'a t = 'a Core_via_map_and_product.t
(** Incarnation of an [Applicative] for an ['a t] with standard
    Requirements ([pure], [map] and [product]).
*)

module Make_via_apply (Core_via_apply : Specs.Applicative.CORE_VIA_APPLY) :
  Specs.APPLICATIVE with type 'a t = 'a Core_via_apply.t
(** Incarnation of an [Applicative] for an ['a t] with standard
    Requirements ([pure] and [apply]).
*)

module Make_core_via_map_and_product
    (Core : Specs.Applicative.CORE_VIA_MAP_AND_PRODUCT) :
  Specs.Applicative.CORE with type 'a t = 'a Core.t
(** Incarnation of an [Applicative.Core] for an ['a t] with standard
    Requirements ([pure], [map] and [product]).
*)

module Make_core_via_apply (Core : Specs.Applicative.CORE_VIA_APPLY) :
  Specs.Applicative.CORE with type 'a t = 'a Core.t
(** Incarnation of an [Applicative.Core] for an ['a t] with standard
    Requirements ([pure], [apply]).
*)

module Make_operation (Core : Specs.Applicative.CORE) :
  Specs.Applicative.OPERATION with type 'a t = 'a Core.t
(** Incarnation of an [Applicative.Operation] for an ['a t] with standard
    Requirements ([pure], [map], [apply] and [product]).
*)

module Make_syntax (Core : Specs.Applicative.CORE) :
  Specs.Applicative.SYNTAX with type 'a t = 'a Core.t
(** Incarnation of an [Applicative.Syntax] for an ['a t] with standard
    Requirements ([pure], [map], [apply] and [product]).
*)

module Make_infix
    (Core : Specs.Applicative.CORE)
    (Operation : Specs.Applicative.OPERATION with type 'a t = 'a Core.t) :
  Specs.Applicative.INFIX with type 'a t = 'a Core.t
(** Incarnation of an [Applicative.Infix] for an ['a t] with standard
    Requirements ([pure], [map], [apply] and [product]).
*)
