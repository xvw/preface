(** {1 Documentation} *)

(** {2 Construction}

    Standard way to build a [Semigroup]. *)

(** Incarnation of a [Semigroup] with [combine]. *)
module Via_combine (Req : Preface_specs.Semigroup.WITH_COMBINE) :
  Preface_specs.SEMIGROUP with type t = Req.t

(** Incarnation of a [Semigroup] from an [Alt]. *)
module From_alt (Alt : Preface_specs.ALT) (T : Preface_specs.Types.T0) :
  Preface_specs.SEMIGROUP with type t = T.t Alt.t

(** Incarnation of a [Semigroup] from an [Alternative]. *)
module From_alternative
    (Alternative : Preface_specs.ALTERNATIVE)
    (T : Preface_specs.Types.T0) :
  Preface_specs.SEMIGROUP with type t = T.t Alternative.t

(** Incarnation of a [Semigroup] from a [Monad_plus]. *)
module From_monad_plus
    (Monad_plus : Preface_specs.MONAD_PLUS)
    (T : Preface_specs.Types.T0) :
  Preface_specs.SEMIGROUP with type t = T.t Monad_plus.t

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

(** Incarnation of a [Semigroup.Core] using [combine]. *)
module Core (Req : Preface_specs.Semigroup.WITH_COMBINE) :
  Preface_specs.Semigroup.CORE with type t = Req.t

(** Incarnation of a [Semigroup.Operation] using [Core]. *)
module Operation (Core : Preface_specs.Semigroup.CORE) :
  Preface_specs.Semigroup.OPERATION with type t = Core.t

(** Incarnation of a [Semigroup.Infix] using [Core]. *)
module Infix (Core : Preface_specs.Semigroup.CORE) :
  Preface_specs.Semigroup.INFIX with type t = Core.t
