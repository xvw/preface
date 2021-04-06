(** {1 Documentation} *)

(** {2 Construction}

    Standard way to build a [Monad]. *)

(** Incarnation of a [Monoid] over a [Semigroup] with [neutral]. *)
module Over_semigroup
    (S : Preface_specs.SEMIGROUP)
    (M : Preface_specs.Monoid.WITH_NEUTRAL with type t = S.t) :
  Preface_specs.MONOID with type t = S.t

(** Incarnation of a [Monoid] with [combine] and [neutral]. *)
module Via_combine_and_neutral
    (Req : Preface_specs.Monoid.WITH_NEUTRAL_AND_COMBINE) :
  Preface_specs.MONOID with type t = Req.t

(** Incarnation of a [Monoid] from an [Alternative]. *)
module From_alternative
    (Alternative : Preface_specs.ALTERNATIVE)
    (T : Preface_specs.Types.T0) :
  Preface_specs.MONOID with type t = T.t Alternative.t

(** Incarnation of a [Monoid] from a [Monad_plus]. *)
module From_monad_plus
    (Monad_plus : Preface_specs.MONAD_PLUS)
    (T : Preface_specs.Types.T0) :
  Preface_specs.MONOID with type t = T.t Monad_plus.t

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
    (M : Preface_specs.Monoid.WITH_NEUTRAL with type t = S.t) :
  Preface_specs.Monoid.CORE with type t = M.t

(** Incarnation of a [Monoid.Core] using [neutral] and [combine]. *)
module Core (Req : Preface_specs.Monoid.WITH_NEUTRAL_AND_COMBINE) :
  Preface_specs.Monoid.CORE with type t = Req.t

(** Incarnation of a [Monoid.Operation] using [Monoid.Core]. *)
module Operation (Core : Preface_specs.Monoid.CORE) :
  Preface_specs.Monoid.OPERATION with type t = Core.t

(** Incarnation of a [Monoid.Infix] using [Monoid.Core]. *)
module Infix (Core : Preface_specs.Monoid.CORE) :
  Preface_specs.Monoid.INFIX with type t = Core.t
