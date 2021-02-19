(** Modules for building {!Preface_specs.ARROW_CHOICE} modules.

    {1 Documentation}

    {2 Construction}

    Standard way to build an [Arrow_choice]. *)

(** Incarnation of an [Arrow_choice] over {!Preface_specs.ARROW} and using
    [left]. *)
module Over_arrow_with_left
    (Arrow : Preface_specs.ARROW)
    (Left : Preface_specs.Arrow_choice.WITH_LEFT
              with type ('a, 'b) t = ('a, 'b) Arrow.t) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) Left.t

(** Incarnation of an [Arrow_choice] over {!Preface_specs.ARROW} and using
    [choose]. *)
module Over_arrow_with_choose
    (Arrow : Preface_specs.ARROW)
    (Choose : Preface_specs.Arrow_choice.WITH_CHOOSE
                with type ('a, 'b) t = ('a, 'b) Arrow.t) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) Choose.t

(** Incarnation of an [Arrow_choice] over {!Preface_specs.ARROW} and using
    [choose] and [left]. *)
module Over_arrow_with_left_and_choose
    (Arrow : Preface_specs.ARROW)
    (Choose_left : Preface_specs.Arrow_choice.WITH_LEFT_AND_CHOOSE
                     with type ('a, 'b) t = ('a, 'b) Arrow.t) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) Choose_left.t

(** Incarnation of an [Arrow_choice] over {!Preface_specs.CATEGORY} and using
    [arrow], [fst] and [left]. *)
module Over_category_and_via_arrow_and_fst_and_left
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_choice.CORE_WITH_ARROW_AND_FST_AND_LEFT
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of an [Arrow_choice] over {!Preface_specs.CATEGORY} and using
    [arrow], [fst] and [choose]. *)
module Over_category_and_via_arrow_and_fst_and_choose
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_choice.CORE_WITH_ARROW_AND_FST_AND_CHOOSE
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of an [Arrow_choice] over {!Preface_specs.CATEGORY} and using
    [arrow], [split] and [left]. *)
module Over_over_category_and_via_arrow_and_split_and_left
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_choice.CORE_WITH_ARROW_AND_SPLIT_AND_LEFT
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of an [Arrow_choice] over {!Preface_specs.CATEGORY} and using
    [arrow], [split] and [choose]. *)
module Over_category_and_via_arrow_and_split_and_choose
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_choice.CORE_WITH_ARROW_AND_SPLIT_AND_CHOOSE
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of an [Arrow_choice] using a [monad] using the Kleisli
    composition. *)
module From_monad (Monad : Preface_specs.Monad.CORE) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = 'a -> 'b Monad.t

(** {2 Manual construction}

    Advanced way to build an [Arrow_plus], constructing and assembling a
    component-by-component an arrow. (In order to provide your own
    implementation for some features.) *)

(** Incarnation of an [Arrow_choice] using each components of a [Arrow_choice]. *)

module Via
    (Core : Preface_specs.Arrow_choice.CORE)
    (Operation : Preface_specs.Arrow_choice.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t)
    (Alias : Preface_specs.Arrow_choice.ALIAS
               with type ('a, 'b) t = ('a, 'b) Operation.t)
    (Infix : Preface_specs.Arrow_choice.INFIX
               with type ('a, 'b) t = ('a, 'b) Alias.t) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) Infix.t

(** Incarnation of an [Arrow_choice.Core] over {!Preface_specs.CATEGORY} and
    using [arrow], [fst] and [left]. *)
module Core_over_category_and_via_arrow_and_fst_and_left
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_choice.CORE_WITH_ARROW_AND_FST_AND_LEFT
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_choice.CORE with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of an [Arrow_choice.Core] over {!Preface_specs.CATEGORY} and
    using [arrow], [split] and [left]. *)
module Core_over_category_and_via_arrow_and_split_and_left
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_choice.CORE_WITH_ARROW_AND_SPLIT_AND_LEFT
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_choice.CORE with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of an [Arrow_choice.Core] over {!Preface_specs.CATEGORY} and
    using [arrow], [fst] and [choose]. *)
module Core_over_category_and_via_arrow_and_fst_and_choose
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_choice.CORE_WITH_ARROW_AND_FST_AND_CHOOSE
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_choice.CORE with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of an [Arrow_choice.Core] over {!Preface_specs.CATEGORY} and
    using [arrow], [split] and [choose]. *)
module Core_over_category_and_via_arrow_and_split_and_choose
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_choice.CORE_WITH_ARROW_AND_SPLIT_AND_CHOOSE
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_choice.CORE with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of an [Arrow_choice.Operation] over {!Preface_specs.CATEGORY}
    and [Core]. *)
module Operation_over_category
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_choice.CORE
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_choice.OPERATION with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of an [Arrow_choice.Alias] using [Operation]. *)
module Alias (Operation : Preface_specs.Arrow.OPERATION) :
  Preface_specs.Arrow.ALIAS with type ('a, 'b) t = ('a, 'b) Operation.t

(** Incarnation of an [Arrow_choice.Infix] over {!Preface_specs.CATEGORY} and
    [Core] and [Operation]. *)
module Infix_over_category
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow_choice.CORE
              with type ('a, 'b) t = ('a, 'b) Category.t)
    (Operation : Preface_specs.Arrow_choice.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) :
  Preface_specs.Arrow_choice.INFIX with type ('a, 'b) t = ('a, 'b) Core.t
