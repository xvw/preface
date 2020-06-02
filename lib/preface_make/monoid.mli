(** {1 Documentation} *)

(** {2 Construction}

    Standard way to build a [Monad]. *)

(** Incarnation of a [Monoid] over a [Semigroup] with [neutral]. *)
module Over_semigroup
    (S : Preface_specs.SEMIGROUP)
    (M : Preface_specs.Monoid.NEUTRAL with type t = S.t) :
  Preface_specs.MONOID with type t = S.t

(** Incarnation of a [Monoid] with [combine] and [neutral]. *)
module Via_combine_and_neutral (Core : Preface_specs.Monoid.CORE) :
  Preface_specs.MONOID with type t = Core.t

(** {2 Manual construction}

    Advanced way to build a [Monoid], constructing and assembling a
    component-by-component a monoid. (In order to provide your own
    implementation for some features.) *)

(** Incarnation of a [Monoid] using each components of a [Monoid]. *)
module Via
    (Core : Preface_specs.Monoid.CORE)
    (Operation : Preface_specs.Monoid.OPERATION with type t = Core.t)
    (Infix : Preface_specs.Monoid.INFIX with type t = Operation.t) :
  Preface_specs.MONOID with type t = Infix.t

(** Incarnation of a [Monoid.Core] over a [Semigroup] with [neutral]. *)
module Core_over_semigroup
    (S : Preface_specs.SEMIGROUP)
    (M : Preface_specs.Monoid.NEUTRAL with type t = S.t) :
  Preface_specs.Monoid.CORE with type t = M.t

(** Incarnation of a [Monoid.Operation] using [Monoid.Core]. *)
module Operation (Core : Preface_specs.Monoid.CORE) :
  Preface_specs.Monoid.OPERATION with type t = Core.t

(** Incarnation of a [Monoid.Infix] using [Monoid.Core]. *)
module Infix (Core : Preface_specs.Monoid.CORE) :
  Preface_specs.Monoid.INFIX with type t = Core.t
