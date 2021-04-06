(** Modules for building {!Preface_specs.ARROW_APPLY} modules.

    {1 Documentation}

    {2 Construction}

    Standard way to build an [Arrow_apply]. *)

(** Incarnation of an [Arrow_apply] over {!Preface_specs.ARROW} and using
    [apply]. *)
module Over_arrow_with_apply
    (Arrow : Preface_specs.ARROW)
    (Apply : Preface_specs.Arrow_apply.WITH_APPLY
               with type ('a, 'b) t = ('a, 'b) Arrow.t) :
  Preface_specs.ARROW_APPLY with type ('a, 'b) t = ('a, 'b) Apply.t

(** Incarnation of an [Arrow_apply] over {!Preface_specs.CATEGORY} and using
    [arrow], [fst] and [apply]. *)
module Over_category_and_via_arrow_and_fst_and_apply
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_apply.WITH_ARROW_AND_FST
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_APPLY with type ('a, 'b) t = ('a, 'b) Req.t

(** Incarnation of an [Arrow_apply] over {!Preface_specs.CATEGORY} and using
    [arrow], [split] and [apply]. *)
module Over_category_and_via_arrow_and_split_and_apply
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_apply.WITH_ARROW_AND_SPLIT
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_APPLY with type ('a, 'b) t = ('a, 'b) Req.t

(** Incarnation of an [Arrow_apply] using a [monad] using the Kleisli
    composition. *)
module From_monad (Monad : Preface_specs.Monad.CORE) :
  Preface_specs.ARROW_APPLY with type ('a, 'b) t = 'a -> 'b Monad.t

(** {2 Manual construction}

    Advanced way to build an [Arrow_apply], constructing and assembling a
    component-by-component an arrow. (In order to provide your own
    implementation for some features.) *)

(** Incarnation of an [Arrow_apply] using each components of a [Arrow_apply]. *)
module Via
    (Core : Preface_specs.Arrow_apply.CORE)
    (Operation : Preface_specs.Arrow_apply.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t)
    (Alias : Preface_specs.Arrow_apply.ALIAS
               with type ('a, 'b) t = ('a, 'b) Operation.t)
    (Infix : Preface_specs.Arrow_apply.INFIX
               with type ('a, 'b) t = ('a, 'b) Alias.t) :
  Preface_specs.ARROW_APPLY with type ('a, 'b) t = ('a, 'b) Infix.t

(** Incarnation of [Arrow_apply.Core] using a [Category], [arrow] and [fst]. *)
module Core_over_category_and_via_arrow_and_fst_and_apply
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_apply.WITH_ARROW_AND_FST
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_apply.CORE with type ('a, 'b) t = ('a, 'b) Req.t

(** Incarnation of [Arrow_apply.Core] using a [Category], [arrow] and [split]. *)
module Core_over_category_and_via_arrow_and_split_and_apply
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_apply.WITH_ARROW_AND_SPLIT
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_apply.CORE with type ('a, 'b) t = ('a, 'b) Req.t

(** Incarnation of [Arrow_apply.Operation] using a [Category] and [Core]. *)
module Operation_over_category
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_apply.CORE
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_apply.OPERATION with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of [Arrow_apply.Alias] using [Operation]. *)
module Alias (Operation : Preface_specs.Arrow_apply.OPERATION) :
  Preface_specs.Arrow_apply.ALIAS with type ('a, 'b) t = ('a, 'b) Operation.t

(** Incarnation of [Arrow_apply.Infix] using a [Category], [Core] and
    [Operation]. *)
module Infix_over_category
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_apply.CORE
              with type ('a, 'b) t = ('a, 'b) Category.t)
    (Operation : Preface_specs.Arrow_apply.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) :
  Preface_specs.Arrow_apply.INFIX with type ('a, 'b) t = ('a, 'b) Operation.t
