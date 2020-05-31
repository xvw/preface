(** {1 Documentation} *)

(** {2 Construction}

    Standard way to build a [Semigroup]. *)

(** Incarnation of a [Semigroup] with [combine]. *)
module Via_combine (Core : Preface_specs.Semigroup.CORE) :
  Preface_specs.SEMIGROUP with type t = Core.t

(** {2 Manual construction}

    Advanced way to build a [Semigroup], constructing and assembling a
    component-by-component a semigroup. (In order to provide your own
    implementation for some features.) *)

(** Incarnation of a [Semigroup] using each components of a [Semigroup]. *)
module Via
    (Core : Preface_specs.Semigroup.CORE)
    (Operation : Preface_specs.Semigroup.OPERATION with type t = Core.t)
    (Infix : Preface_specs.Semigroup.INFIX with type t = Operation.t) :
  Preface_specs.SEMIGROUP with type t = Infix.t

(** Incarnation of a [Semigroup.Operation] using [Core]. *)
module Operation (Core : Preface_specs.Semigroup.CORE) :
  Preface_specs.Semigroup.OPERATION with type t = Core.t

(** Incarnation of a [Semigroup.Infix] using [Core]. *)
module Infix (Core : Preface_specs.Semigroup.CORE) :
  Preface_specs.Semigroup.INFIX with type t = Core.t
