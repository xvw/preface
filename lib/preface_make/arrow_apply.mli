(** Building a {!module:Preface_specs.Arrow_apply} *)

(** {1 Using the minimal definition} *)

(** {2 Using apply over an Arrow}

    Build an {!module-type:Preface_specs.ARROW_APPLY} using
    {!module-type:Preface_specs.Arrow_apply.WITH_APPLY} over an
    {!module-type:Preface_specs.ARROW}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_arrow
    (Arrow : Preface_specs.ARROW)
    (Apply : Preface_specs.Arrow_apply.WITH_APPLY
               with type ('a, 'b) t = ('a, 'b) Arrow.t) :
  Preface_specs.ARROW_APPLY with type ('a, 'b) t = ('a, 'b) Apply.t

(** {2 Using apply, arrow and fst over a Category}

    Build an {!module-type:Preface_specs.ARROW_APPLY} using
    {!module-type:Preface_specs.Arrow_apply.WITH_ARROW_AND_FST} over a
    {!module-type:Preface_specs.CATEGORY}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_category_and_via_arrow_and_fst
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_apply.WITH_ARROW_AND_FST
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_APPLY with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Using apply, arrow and split over a Category}

    Build an {!module-type:Preface_specs.ARROW_APPLY} using
    {!module-type:Preface_specs.Arrow_apply.WITH_ARROW_AND_SPLIT} over a
    {!module-type:Preface_specs.CATEGORY}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_category_and_via_arrow_and_split
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_apply.WITH_ARROW_AND_SPLIT
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_APPLY with type ('a, 'b) t = ('a, 'b) Req.t

(** {1 From other abstraction} *)

(** {2 From a Monad}

    Produces an {!module-type:Preface_specs.ARROW_APPLY} from a
    {!module-type:Preface_specs.MONAD} (using the [Kleisli Arrow]). *)

module From_monad (Monad : Preface_specs.Monad.CORE) :
  Preface_specs.ARROW_APPLY with type ('a, 'b) t = 'a -> 'b Monad.t

(** {1 Manual construction}

    Advanced way to build an {!module-type:Preface_specs.ARROW_APPLY},
    constructing and assembling a component-by-component of
    {!module-type:Preface_specs.ARROW_APPLY}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Arrow_apply.CORE)
    (Operation : Preface_specs.Arrow_apply.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t)
    (Alias : Preface_specs.Arrow_apply.ALIAS
               with type ('a, 'b) t = ('a, 'b) Operation.t)
    (Infix : Preface_specs.Arrow_apply.INFIX
               with type ('a, 'b) t = ('a, 'b) Alias.t) :
  Preface_specs.ARROW_APPLY with type ('a, 'b) t = ('a, 'b) Infix.t

(** {2 Building Core} *)

module Core_over_category_and_via_arrow_and_fst
    (Category : Preface_specs.Category.CORE)
    (Req : Preface_specs.Arrow_apply.WITH_ARROW_AND_FST
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_apply.CORE with type ('a, 'b) t = ('a, 'b) Req.t

module Core_over_category_and_via_arrow_and_split
    (Category : Preface_specs.Category.CORE)
    (Req : Preface_specs.Arrow_apply.WITH_ARROW_AND_SPLIT
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_apply.CORE with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Deriving Operation} *)

module Operation_over_category
    (Category : Preface_specs.Category.OPERATION)
    (Core : Preface_specs.Arrow_apply.CORE
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_apply.OPERATION with type ('a, 'b) t = ('a, 'b) Core.t

(** {2 Deriving Alias} *)

module Alias (Operation : Preface_specs.Arrow_apply.OPERATION) :
  Preface_specs.Arrow_apply.ALIAS with type ('a, 'b) t = ('a, 'b) Operation.t

(** {2 Deriving Infix} *)

module Infix_over_category
    (Category : Preface_specs.Category.INFIX)
    (Core : Preface_specs.Arrow_apply.CORE
              with type ('a, 'b) t = ('a, 'b) Category.t)
    (Operation : Preface_specs.Arrow_apply.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) :
  Preface_specs.Arrow_apply.INFIX with type ('a, 'b) t = ('a, 'b) Operation.t
