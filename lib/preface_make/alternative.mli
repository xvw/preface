(** Modules for building {!Preface_specs.ALTERNATIVE} modules.

    {1 Documentation} *)

(** {2 Construction}

    Standard way to build an [Alternative]. *)

(** Incarnation of an [Alternative] with standard requirements ([pure], [map],
    [product], [neutral] and [combine]). *)
module Via_map_and_product
    (Core_with_map_and_product : Preface_specs.Alternative
                                 .CORE_WITH_MAP_AND_PRODUCT) :
  Preface_specs.ALTERNATIVE with type 'a t = 'a Core_with_map_and_product.t

(** Incarnation of an [Alternative] with standard requirements ([pure], [apply],
    [neutral] and [combine]). *)
module Via_apply (Core_with_apply : Preface_specs.Alternative.CORE_WITH_APPLY) :
  Preface_specs.ALTERNATIVE with type 'a t = 'a Core_with_apply.t

(** Incarnation of an [Alternative] over an [Applicative].*)
module Over_applicative
    (Applicative : Preface_specs.APPLICATIVE)
    (Core : Preface_specs.Alternative.CORE_WITH_NEUTRAL_AND_COMBINE
              with type 'a t = 'a Applicative.t) :
  Preface_specs.ALTERNATIVE with type 'a t = 'a Core.t

(** {2 Manual construction}

    Advanced way to build an [Alternative], constructing and assembling a
    component-by-component an alternative. (In order to provide your own
    implementation for some features.) *)

(** Incarnation of an [Alternative] using each components of an [Alternative]. *)
module Via
    (Core : Preface_specs.Alternative.CORE)
    (Operation : Preface_specs.Alternative.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Alternative.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Alternative.SYNTAX with type 'a t = 'a Core.t) :
  Preface_specs.ALTERNATIVE with type 'a t = 'a Core.t

(** Incarnation of an [Alternative.Core] with standard requirements ([pure],
    [map], [product], [neutral] and [combine]). *)
module Core_via_map_and_product
    (Core : Preface_specs.Alternative.CORE_WITH_MAP_AND_PRODUCT) :
  Preface_specs.Alternative.CORE with type 'a t = 'a Core.t

(** Incarnation of an [Alternative.Core] with standard requirements ([pure],
    [apply], [neutral] and [combine]). *)
module Core_via_apply (Core : Preface_specs.Alternative.CORE_WITH_APPLY) :
  Preface_specs.Alternative.CORE with type 'a t = 'a Core.t

(** Incarnation of an [Alternative.Operation] with standard requirements
    ([pure], [map], [apply], [product], [neutral] and [combine]). *)
module Operation (Core : Preface_specs.Alternative.CORE) :
  Preface_specs.Alternative.OPERATION with type 'a t = 'a Core.t

(** Incarnation of an [Alternative.Syntax] with standard requirements ([pure],
    [map], [apply], [product], [neutral] and [combine]). *)
module Syntax (Core : Preface_specs.Alternative.CORE) :
  Preface_specs.Alternative.SYNTAX with type 'a t = 'a Core.t

(** Incarnation of an [Alternative.Infix] with standard requirements ([pure],
    [map], [apply], [product], [neutral] and [combine]). *)
module Infix
    (Core : Preface_specs.Alternative.CORE)
    (Operation : Preface_specs.Alternative.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Alternative.INFIX with type 'a t = 'a Core.t
