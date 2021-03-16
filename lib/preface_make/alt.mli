(** {1 Documentation} *)

(** {2 Construction}

    Standard way to build a [Alt]. *)

(** Incarnation of a [Alt] with [combine] and [map]. *)
module Via_map_and_combine (Core : Preface_specs.Alt.CORE) :
  Preface_specs.ALT with type 'a t = 'a Core.t

(** Incarnation of a [Alt] over a [Functor] and via the [combine] operation. *)
module Over_functor_via_combine
    (Functor : Preface_specs.FUNCTOR)
    (Combine : Preface_specs.Alt.WITH_COMBINE with type 'a t = 'a Functor.t) :
  Preface_specs.ALT with type 'a t = 'a Combine.t

(** {2 Manual construction}

    Advanced way to build an [Alt], constructing and assembling a
    component-by-component an alt. (In order to provide your own implementation
    for some features.) *)

(** Incarnation of a [Alt] using each components of an [Alt]. *)
module Via
    (Core : Preface_specs.Alt.CORE)
    (Operation : Preface_specs.Alt.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Alt.INFIX with type 'a t = 'a Operation.t) :
  Preface_specs.ALT with type 'a t = 'a Infix.t

(** Incarnation of an [Alt.Operation] using [Core]. *)
module Operation (Core : Preface_specs.Alt.CORE) :
  Preface_specs.Alt.OPERATION with type 'a t = 'a Core.t

(** Incarnation of an [Alt.Infix] using [Core]. *)
module Infix
    (Core : Preface_specs.Alt.CORE)
    (Operation : Preface_specs.Alt.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Alt.INFIX with type 'a t = 'a Core.t
