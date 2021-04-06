(** Modules for building {!Preface_specs.ARROW_PLUS} modules.

    {1 Documentation}

    {2 Construction}

    Standard way to build an [Arrow_plus]. *)

(** Incarnation of an [Arrow_plus] over {!Preface_specs.ARROW}. *)
module Over_arrow
    (Arrow : Preface_specs.ARROW)
    (Req : Preface_specs.Arrow_plus.WITH_COMBINE_AND_NEUTRAL
             with type ('a, 'b) t = ('a, 'b) Arrow.t) :
  Preface_specs.ARROW_PLUS with type ('a, 'b) t = ('a, 'b) Req.t

(** Incarnation of an [Arrow_plus] using a {!Preface_specs.CATEGORY}, [arrow],
    [fst] and [combine]. *)
module Over_category_and_via_arrow_and_fst
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_plus.WITH_ARROW_AND_FST
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_PLUS with type ('a, 'b) t = ('a, 'b) Req.t

(** Incarnation of an [Arrow] using a {!Preface_specs.CATEGORY}, [arrow],
    [split] and [combine]. *)
module Over_category_and_via_arrow_and_split
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_plus.WITH_ARROW_AND_SPLIT
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_PLUS with type ('a, 'b) t = ('a, 'b) Req.t

(** Incarnation of an [Arrow_plus] using a [monad_plus] using the Kleisli
    composition. *)
module From_monad_plus (Monad : Preface_specs.Monad_plus.CORE) :
  Preface_specs.ARROW_PLUS with type ('a, 'b) t = 'a -> 'b Monad.t

(** {2 Arrow plus composition}

    Some tools for composition between arrows plus. *)

(** Product of two Arrows plus. *)
module Product (F : Preface_specs.ARROW_PLUS) (G : Preface_specs.ARROW_PLUS) :
  Preface_specs.ARROW_PLUS with type ('a, 'b) t = ('a, 'b) F.t * ('a, 'b) G.t

(** {2 Manual construction}

    Advanced way to build an [Arrow_plus], constructing and assembling a
    component-by-component an arrow. (In order to provide your own
    implementation for some features.) *)

(** Incarnation of an [Arrow_plus] using each components of a [Arrow_plus]. *)
module Via
    (Core : Preface_specs.Arrow_plus.CORE)
    (Operation : Preface_specs.Arrow_plus.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t)
    (Alias : Preface_specs.Arrow_plus.ALIAS
               with type ('a, 'b) t = ('a, 'b) Operation.t)
    (Infix : Preface_specs.Arrow_plus.INFIX
               with type ('a, 'b) t = ('a, 'b) Alias.t) :
  Preface_specs.ARROW_PLUS with type ('a, 'b) t = ('a, 'b) Infix.t

(** Incarnation of [Arrow_plus.Core] using a [Category], [arrow], [fst] and
    [combine]. *)
module Core_over_category_and_via_arrow_and_fst
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_plus.WITH_ARROW_AND_FST
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_plus.CORE with type ('a, 'b) t = ('a, 'b) Req.t

(** Incarnation of [Arrow_plus.Core] using a [Category], [arrow] and [split]. *)
module Core_over_category_and_via_arrow_and_split
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_plus.WITH_ARROW_AND_SPLIT
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_plus.CORE with type ('a, 'b) t = ('a, 'b) Req.t

(** Incarnation of [Arrow_plus.Operation] using a [Category] and [Core]. *)
module Operation_over_category
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_plus.CORE
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_plus.OPERATION with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of [Arrow_plus.Alias] using [Operation]. *)
module Alias (Operation : Preface_specs.Arrow_plus.OPERATION) :
  Preface_specs.Arrow_plus.ALIAS with type ('a, 'b) t = ('a, 'b) Operation.t

(** Incarnation of [Arrow_plus.Infix] using a [Category], [Core] and
    [Operation]. *)
module Infix_over_category
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_plus.CORE
              with type ('a, 'b) t = ('a, 'b) Category.t)
    (Operation : Preface_specs.Arrow_plus.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) :
  Preface_specs.Arrow_plus.INFIX with type ('a, 'b) t = ('a, 'b) Operation.t
