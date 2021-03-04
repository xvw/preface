(** Modules for building {!Preface_specs.ARROW_LOOP} modules.

    {1 Documentation}

    {2 Construction}

    Standard way to build an [Arrow_loop]. *)

(** Incarnation of an [Arrow_loop] over {!Preface_specs.ARROW} and using [loop]. *)
module Over_arrow_with_loop
    (Arrow : Preface_specs.ARROW)
    (Loop : Preface_specs.Arrow_loop.WITH_LOOP
              with type ('a, 'b) t = ('a, 'b) Arrow.t) :
  Preface_specs.ARROW_LOOP with type ('a, 'b) t = ('a, 'b) Loop.t

(** Incarnation of an [Arrow_loop] over {!Preface_specs.CATEGORY} and using
    [arrow], [fst] and [loop]. *)
module Over_category_and_via_arrow_and_fst_and_loop
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_loop.CORE_WITH_ARROW_AND_FST_AND_LOOP
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_LOOP with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of an [Arrow_loop] over {!Preface_specs.CATEGORY} and using
    [arrow], [split] and [loop]. *)
module Over_category_and_via_arrow_and_split_and_loop
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_loop.CORE_WITH_ARROW_AND_SPLIT_AND_LOOP
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_LOOP with type ('a, 'b) t = ('a, 'b) Core.t

(** {2 Manual construction}

    Advanced way to build an [Arrow_loop], constructing and assembling a
    component-by-component an arrow. (In order to provide your own
    implementation for some features.) *)

(** Incarnation of an [Arrow_loop] using each components of a [Arrow_loop]. *)
module Via
    (Core : Preface_specs.Arrow_loop.CORE)
    (Operation : Preface_specs.Arrow_loop.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t)
    (Alias : Preface_specs.Arrow_loop.ALIAS
               with type ('a, 'b) t = ('a, 'b) Operation.t)
    (Infix : Preface_specs.Arrow_loop.INFIX
               with type ('a, 'b) t = ('a, 'b) Alias.t) :
  Preface_specs.ARROW_LOOP with type ('a, 'b) t = ('a, 'b) Infix.t

(** Incarnation of [Arrow_loop.Core] using a [Category], [arrow] and [fst]. *)
module Core_over_category_and_via_arrow_and_fst_and_loop
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_loop.CORE_WITH_ARROW_AND_FST_AND_LOOP
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_loop.CORE with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of [Arrow_loop.Core] using a [Category], [arrow] and [split]. *)
module Core_over_category_and_via_arrow_and_split_and_loop
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_loop.CORE_WITH_ARROW_AND_SPLIT_AND_LOOP
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_loop.CORE with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of [Arrow_loop.Operation] using a [Category] and [Core]. *)
module Operation_over_category
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_loop.CORE
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_loop.OPERATION with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of [Arrow_loop.Alias] using [Operation]. *)
module Alias (Operation : Preface_specs.Arrow_loop.OPERATION) :
  Preface_specs.Arrow_loop.ALIAS with type ('a, 'b) t = ('a, 'b) Operation.t

(** Incarnation of [Arrow_loop.Infix] using a [Category], [Core] and
    [Operation]. *)
module Infix_over_category
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_loop.CORE
              with type ('a, 'b) t = ('a, 'b) Category.t)
    (Operation : Preface_specs.Arrow_loop.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) :
  Preface_specs.Arrow_loop.INFIX with type ('a, 'b) t = ('a, 'b) Operation.t
