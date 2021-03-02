(** Modules for building {!Preface_specs.ARROW} modules.

    {1 Documentation}

    {2 Construction}

    Standard way to build an [Arrow]. *)

(** Incarnation of an [Arrow] using a {!Preface_specs.CATEGORY}, [arrow] and
    [fst]. *)
module Over_category_and_via_arrow_and_fst
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow.CORE_WITH_ARROW_AND_FST
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of an [Arrow] using a {!Preface_specs.CATEGORY}, [arrow] and
    [split]. *)
module Over_category_and_via_arrow_an_split
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow.CORE_WITH_ARROW_AND_SPLIT
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of an [Arrow] using a [monad] using the Kleisli composition. *)
module From_monad (Monad : Preface_specs.Monad.CORE) :
  Preface_specs.ARROW with type ('a, 'b) t = 'a -> 'b Monad.t

(** Incarnation of an [Arrow] using from an [Arrow_plus]. *)
module From_arrow_plus (Plus : Preface_specs.ARROW_PLUS) :
  Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) Plus.t

(** Incarnation of an [Arrow] using from an [Arrow_alt]. *)
module From_arrow_alt (Alt : Preface_specs.ARROW_ALT) :
  Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) Alt.t

(** Incarnation of an [Arrow] using from an [Arrow_zero]. *)
module From_arrow_zero (Zero : Preface_specs.ARROW_ZERO) :
  Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) Zero.t

(** Incarnation of an [Arrow] using from an [Arrow_choice]. *)
module From_arrow_choice (Choice : Preface_specs.ARROW_CHOICE) :
  Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) Choice.t

(** {2 Manual construction}

    Advanced way to build an [Arrow], constructing and assembling a
    component-by-component an arrow. (In order to provide your own
    implementation for some features.) *)

(** Incarnation of an [Arrow] using each components of a [Arrow]. *)
module Via
    (Core : Preface_specs.Arrow.CORE)
    (Operation : Preface_specs.Arrow.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t)
    (Alias : Preface_specs.Arrow.ALIAS
               with type ('a, 'b) t = ('a, 'b) Operation.t)
    (Infix : Preface_specs.Arrow.INFIX with type ('a, 'b) t = ('a, 'b) Alias.t) :
  Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) Infix.t

(** Incarnation of [Arrow.Core] using a [Category], [arrow] and [fst]. *)
module Core_over_category_and_via_arrow_and_fst
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow.CORE_WITH_ARROW_AND_FST
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow.CORE with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of [Arrow.Core] using a [Category], [arrow] and [split]. *)
module Core_over_category_and_via_arrow_and_split
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow.CORE_WITH_ARROW_AND_SPLIT
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow.CORE with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of [Arrow.Operation] using a [Category] and [Core]. *)
module Operation_over_category
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow.CORE with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow.OPERATION with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of [Arrow.Alias] using [Operation]. *)
module Alias (Operation : Preface_specs.Arrow.OPERATION) :
  Preface_specs.Arrow.ALIAS with type ('a, 'b) t = ('a, 'b) Operation.t

(** Incarnation of [Arrow.Infix] using a [Category], [Core] and [Operation]. *)
module Infix_over_category
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow.CORE with type ('a, 'b) t = ('a, 'b) Category.t)
    (Operation : Preface_specs.Arrow.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) :
  Preface_specs.Arrow.INFIX with type ('a, 'b) t = ('a, 'b) Operation.t
