(** Building a {!module:Preface_specs.Monoid} *)

(** {1 Using the minimal definition} *)

(** {2 Using neutral and combine}

    Build a {!module-type:Preface_specs.MONOID} using
    {!module-type:Preface_specs.Monoid.WITH_NEUTRAL_AND_COMBINE}.

    Standard method, using the minimal definition of a monoid to derive its full
    API. *)

module Via_combine_and_neutral
    (Req : Preface_specs.Monoid.WITH_NEUTRAL_AND_COMBINE) :
  Preface_specs.MONOID with type t = Req.t

(** {2 Over a Semigroup}

    Build a {!module-type:Preface_specs.MONOID} over a
    {!module-type:Preface_specs.SEMIGROUP}.

    If you already have a Semigroup, you can enrich it by passing only the
    [neutral] function. *)

module Over_semigroup
    (S : Preface_specs.SEMIGROUP)
    (M : Preface_specs.Monoid.WITH_NEUTRAL with type t = S.t) :
  Preface_specs.MONOID with type t = S.t

(** {1 From other abstraction} *)

(** {2 From an Alternative}

    Specialize an {!module-type:Preface_specs.ALTERNATIVE} into a
    {!module-type:Preface_specs.MONOID}. *)

module From_alternative
    (Alternative : Preface_specs.ALTERNATIVE)
    (T : Preface_specs.Types.T0) :
  Preface_specs.MONOID with type t = T.t Alternative.t

(** {2 From a Monad plus}

    Specialize an {!module-type:Preface_specs.MONAD_PLUS} into a
    {!module-type:Preface_specs.MONOID}. *)

module From_monad_plus
    (Monad_plus : Preface_specs.MONAD_PLUS)
    (T : Preface_specs.Types.T0) :
  Preface_specs.MONOID with type t = T.t Monad_plus.t

(** {1 Manual construction}

    Advanced way to build a {!module-type:Preface_specs.MONOID}, constructing
    and assembling a component-by-component of
    {!module-type:Preface_specs.MONOID}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Monoid.CORE)
    (Operation : Preface_specs.Monoid.OPERATION with type t = Core.t)
    (Infix : Preface_specs.Monoid.INFIX with type t = Operation.t) :
  Preface_specs.MONOID with type t = Infix.t

(** {2 Building Core} *)

module Core_over_semigroup
    (S : Preface_specs.SEMIGROUP)
    (M : Preface_specs.Monoid.WITH_NEUTRAL with type t = S.t) :
  Preface_specs.Monoid.CORE with type t = M.t

module Core (Req : Preface_specs.Monoid.WITH_NEUTRAL_AND_COMBINE) :
  Preface_specs.Monoid.CORE with type t = Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Monoid.CORE) :
  Preface_specs.Monoid.OPERATION with type t = Core.t

(** {2 Deriving Infix} *)

module Infix (Core : Preface_specs.Monoid.CORE) :
  Preface_specs.Monoid.INFIX with type t = Core.t
