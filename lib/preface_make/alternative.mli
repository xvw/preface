(** Modules for building {!Preface_specs.ALTERNATIVE} modules.

    {1 Documentation} *)

(** {2 Construction}

    Standard way to build an [Alternative]. *)

(** Incarnation of an [Alternative] with standard requirements ([pure], [map],
    [product], [neutral] and [combine]). *)
module Via_map_and_product
    (Req : Preface_specs.Alternative.WITH_MAP_AND_PRODUCT) :
  Preface_specs.ALTERNATIVE with type 'a t = 'a Req.t

(** Incarnation of an [Alternative] with standard requirements ([pure], [apply],
    [neutral] and [combine]). *)
module Via_apply (Req : Preface_specs.Alternative.WITH_APPLY) :
  Preface_specs.ALTERNATIVE with type 'a t = 'a Req.t

(** Incarnation of an [Alternative] over an [Applicative].*)
module Over_applicative
    (Applicative : Preface_specs.APPLICATIVE)
    (Req : Preface_specs.Alternative.WITH_NEUTRAL_AND_COMBINE
             with type 'a t = 'a Applicative.t) :
  Preface_specs.ALTERNATIVE with type 'a t = 'a Req.t

(** Incarnation of an [Alternative] using an [Arrow_plus] via [Arrow Monad]
    encoding.*)
module From_arrow_plus (A : Preface_specs.ARROW_PLUS) :
  Preface_specs.ALTERNATIVE with type 'a t = (unit, 'a) A.t

(** {2 Alternative composition}

    Some tools for composition between alternatives. *)

(** Right-to-left composition of alternative with applicatives.*)
module Composition
    (F : Preface_specs.ALTERNATIVE)
    (G : Preface_specs.APPLICATIVE) :
  Preface_specs.ALTERNATIVE with type 'a t = 'a G.t F.t

(** Product of two Alternatives. *)
module Product (F : Preface_specs.ALTERNATIVE) (G : Preface_specs.ALTERNATIVE) :
  Preface_specs.ALTERNATIVE with type 'a t = 'a F.t * 'a G.t

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
    (Req : Preface_specs.Alternative.WITH_MAP_AND_PRODUCT) :
  Preface_specs.Alternative.CORE with type 'a t = 'a Req.t

(** Incarnation of an [Alternative.Core] with standard requirements ([pure],
    [apply], [neutral] and [combine]). *)
module Core_via_apply (Req : Preface_specs.Alternative.WITH_APPLY) :
  Preface_specs.Alternative.CORE with type 'a t = 'a Req.t

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
