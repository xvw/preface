(** Building a {!module:Preface_specs.Arrow_alt} *)

(** {1 Using the minimal definition} *)

(** {2 Using combine over an Arrow}

    Build an {!module-type:Preface_specs.ARROW_ALT} using
    {!module-type:Preface_specs.Arrow_alt.WITH_COMBINE} over an
    {!module-type:Preface_specs.ARROW}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_arrow
    (Arrow : Preface_specs.ARROW)
    (Req : Preface_specs.Arrow_alt.WITH_COMBINE
             with type ('a, 'b) t = ('a, 'b) Arrow.t) :
  Preface_specs.ARROW_ALT with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Using combine, arrow and fst over a Category}

    Build an {!module-type:Preface_specs.ARROW_ALT} using
    {!module-type:Preface_specs.Arrow_alt.WITH_ARROW_AND_FST} over a
    {!module-type:Preface_specs.CATEGORY}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_category_and_via_arrow_and_fst
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_alt.WITH_ARROW_AND_FST
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_ALT with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Using combine, arrow and split over a Category}

    Build an {!module-type:Preface_specs.ARROW_ALT} using
    {!module-type:Preface_specs.Arrow_alt.WITH_ARROW_AND_SPLIT} over a
    {!module-type:Preface_specs.CATEGORY}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_category_and_via_arrow_and_split
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_alt.WITH_ARROW_AND_SPLIT
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_ALT with type ('a, 'b) t = ('a, 'b) Req.t

(** {1 Arrow Alt Algebra}

    Construction of {!module-type:Preface_specs.ARROW_ALT} by combining them. *)

(** {2 Product}

    Construct the product of two {!module-type:Preface_specs.ARROW_ALT}. *)

module Product (F : Preface_specs.ARROW_ALT) (G : Preface_specs.ARROW_ALT) :
  Preface_specs.ARROW_ALT with type ('a, 'b) t = ('a, 'b) F.t * ('a, 'b) G.t

(** {1 From other abstraction} *)

(** {2 From an Arrow Plus}

    Produces an {!module-type:Preface_specs.ARROW_ALT} from an
    {!module-type:Preface_specs.ARROW_PLUS}. *)

module From_arrow_plus (Plus : Preface_specs.ARROW_PLUS) :
  Preface_specs.ARROW_ALT with type ('a, 'b) t = ('a, 'b) Plus.t

(** {2 From a Monad Plus}

    Produces an {!module-type:Preface_specs.ARROW_ALT} from a
    {!module-type:Preface_specs.MONAD_PLUSD} (using the [Kleisli Arrow]). *)

module From_monad_plus (Monad : Preface_specs.Monad_plus.CORE) :
  Preface_specs.ARROW_ALT with type ('a, 'b) t = 'a -> 'b Monad.t

(** {1 Manual construction}

    Advanced way to build an {!module-type:Preface_specs.ARROW_ALT},
    constructing and assembling a component-by-component of
    {!module-type:Preface_specs.ARROW_ALT}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Arrow_alt.CORE)
    (Operation : Preface_specs.Arrow_alt.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t)
    (Alias : Preface_specs.Arrow_alt.ALIAS
               with type ('a, 'b) t = ('a, 'b) Operation.t)
    (Infix : Preface_specs.Arrow_alt.INFIX
               with type ('a, 'b) t = ('a, 'b) Alias.t) :
  Preface_specs.ARROW_ALT with type ('a, 'b) t = ('a, 'b) Infix.t

(** {2 Building Core} *)

module Core_over_category_and_via_arrow_and_fst
    (Category : Preface_specs.Category.CORE)
    (Req : Preface_specs.Arrow_alt.WITH_ARROW_AND_FST
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_alt.CORE with type ('a, 'b) t = ('a, 'b) Req.t

module Core_over_category_and_via_arrow_and_split
    (Category : Preface_specs.Category.CORE)
    (Req : Preface_specs.Arrow_alt.WITH_ARROW_AND_SPLIT
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_alt.CORE with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Deriving Operation} *)

module Operation_over_category
    (Category : Preface_specs.Category.OPERATION)
    (Core : Preface_specs.Arrow_alt.CORE
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_alt.OPERATION with type ('a, 'b) t = ('a, 'b) Core.t

(** {2 Deriving Alias} *)

module Alias (Operation : Preface_specs.Arrow_alt.OPERATION) :
  Preface_specs.Arrow_alt.ALIAS with type ('a, 'b) t = ('a, 'b) Operation.t

(** {2 Deriving Infix} *)

module Infix_over_category
    (Category : Preface_specs.Category.INFIX)
    (Core : Preface_specs.Arrow_alt.CORE
              with type ('a, 'b) t = ('a, 'b) Category.t)
    (Operation : Preface_specs.Arrow_alt.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) :
  Preface_specs.Arrow_alt.INFIX with type ('a, 'b) t = ('a, 'b) Operation.t
