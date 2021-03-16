(** Modules for building {!Preface_specs.ARROW_ZERO} modules.

    {1 Documentation}

    {2 Construction}

    Standard way to build an [Arrow_zero]. *)

(** Incarnation of an [Arrow_zero] over {!Preface_specs.ARROW}. *)
module Over_arrow
    (Arrow : Preface_specs.ARROW)
    (Neutral : Preface_specs.Arrow_zero.NEUTRAL
                 with type ('a, 'b) t = ('a, 'b) Arrow.t) :
  Preface_specs.ARROW_ZERO with type ('a, 'b) t = ('a, 'b) Neutral.t

(** Incarnation of an [Arrow_zero] from {!Preface_specs.ARROW_PLUS}. *)
module From_arrow_plus (Plus : Preface_specs.ARROW_PLUS) :
  Preface_specs.ARROW_ZERO with type ('a, 'b) t = ('a, 'b) Plus.t

(** Incarnation of an [Arrow_zero] using a {!Preface_specs.CATEGORY}, [arrow],
    [fst] and [neutral]. *)
module Over_category_and_via_arrow_and_fst
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_zero.CORE_WITH_ARROW_AND_FST
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_ZERO with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of an [Arrow] using a {!Preface_specs.CATEGORY}, [arrow],
    [split] and [neutral]. *)
module Over_category_and_via_arrow_an_split
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_zero.CORE_WITH_ARROW_AND_SPLIT
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_ZERO with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of an [Arrow_zero] using a [monad_plus] using the Kleisli
    composition. *)
module From_monad_plus (Monad : Preface_specs.Monad_plus.CORE) :
  Preface_specs.ARROW_ZERO with type ('a, 'b) t = 'a -> 'b Monad.t

(** {2 Arrow Zero composition}

    Some tools for composition between arrows zero. *)

(** Product of two Arrows zero. *)
module Product (F : Preface_specs.ARROW_ZERO) (G : Preface_specs.ARROW_ZERO) :
  Preface_specs.ARROW_ZERO with type ('a, 'b) t = ('a, 'b) F.t * ('a, 'b) G.t

(** {2 Manual construction}

    Advanced way to build an [Arrow_zero], constructing and assembling a
    component-by-component an arrow. (In order to provide your own
    implementation for some features.) *)

(** Incarnation of an [Arrow_zero] using each components of a [Arrow_zero]. *)
module Via
    (Core : Preface_specs.Arrow_zero.CORE)
    (Operation : Preface_specs.Arrow_zero.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t)
    (Alias : Preface_specs.Arrow_zero.ALIAS
               with type ('a, 'b) t = ('a, 'b) Operation.t)
    (Infix : Preface_specs.Arrow_zero.INFIX
               with type ('a, 'b) t = ('a, 'b) Alias.t) :
  Preface_specs.ARROW_ZERO with type ('a, 'b) t = ('a, 'b) Infix.t

(** Incarnation of [Arrow_zero.Core] using a [Category], [arrow], [fst] and
    [neutral]. *)
module Core_over_category_and_via_arrow_and_fst
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_zero.CORE_WITH_ARROW_AND_FST
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_zero.CORE with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of [Arrow_zero.Core] using a [Category], [arrow] and [split]. *)
module Core_over_category_and_via_arrow_and_split
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_zero.CORE_WITH_ARROW_AND_SPLIT
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_zero.CORE with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of [Arrow_zero.Operation] using a [Category] and [Core]. *)
module Operation_over_category
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_zero.CORE
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_zero.OPERATION with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of [Arrow_zero.Alias] using [Operation]. *)
module Alias (Operation : Preface_specs.Arrow_zero.OPERATION) :
  Preface_specs.Arrow_zero.ALIAS with type ('a, 'b) t = ('a, 'b) Operation.t

(** Incarnation of [Arrow_zero.Infix] using a [Category], [Core] and
    [Operation]. *)
module Infix_over_category
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_zero.CORE
              with type ('a, 'b) t = ('a, 'b) Category.t)
    (Operation : Preface_specs.Arrow_zero.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) :
  Preface_specs.Arrow_zero.INFIX with type ('a, 'b) t = ('a, 'b) Operation.t
