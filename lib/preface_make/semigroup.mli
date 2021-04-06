(** Building a {!module:Preface_specs.Semigroup} *)

(** {1 Using the minimal definition}

    Build a {!module-type:Preface_specs.SEMIGROUP} using
    {!module-type:Preface_specs.Semigroup.WITH_COMBINE}.

    Standard method, using the minimal definition of a semigroup to derive its
    full API. *)

module Via_combine (Req : Preface_specs.Semigroup.WITH_COMBINE) :
  Preface_specs.SEMIGROUP with type t = Req.t

(** {1 From other abstraction} *)

(** {2 From an Alt}

    Specialize an {!module-type:Preface_specs.ALT} into a
    {!module-type:Preface_specs.SEMIGROUP}. *)

module From_alt (Alt : Preface_specs.ALT) (T : Preface_specs.Types.T0) :
  Preface_specs.SEMIGROUP with type t = T.t Alt.t

(** {2 From an Alternative}

    Specialize an {!module-type:Preface_specs.ALTERNATIVE} into a
    {!module-type:Preface_specs.SEMIGROUP}. *)

module From_alternative
    (Alternative : Preface_specs.ALTERNATIVE)
    (T : Preface_specs.Types.T0) :
  Preface_specs.SEMIGROUP with type t = T.t Alternative.t

(** {2 From a Monad plus}

    Specialize an {!module-type:Preface_specs.MONAD_PLUS} into a
    {!module-type:Preface_specs.SEMIGROUP}. *)

module From_monad_plus
    (Monad_plus : Preface_specs.MONAD_PLUS)
    (T : Preface_specs.Types.T0) :
  Preface_specs.SEMIGROUP with type t = T.t Monad_plus.t

(** {1 Manual construction}

    Advanced way to build a {!module-type:Preface_specs.SEMIGROUP}, constructing
    and assembling a component-by-component of
    {!module-type:Preface_specs.SEMIGROUP}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Semigroup.CORE)
    (Operation : Preface_specs.Semigroup.OPERATION with type t = Core.t)
    (Infix : Preface_specs.Semigroup.INFIX with type t = Operation.t) :
  Preface_specs.SEMIGROUP with type t = Infix.t

(** {2 Building Core} *)

module Core (Req : Preface_specs.Semigroup.WITH_COMBINE) :
  Preface_specs.Semigroup.CORE with type t = Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Semigroup.CORE) :
  Preface_specs.Semigroup.OPERATION with type t = Core.t

(** {2 Deriving Infix} *)

module Infix (Core : Preface_specs.Semigroup.CORE) :
  Preface_specs.Semigroup.INFIX with type t = Core.t
