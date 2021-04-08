(** Building a {!module:Preface_specs.Arrow} *)

(** {1 Using the minimal definition} *)

(** {2 Using arrow and fst over a Category}

    Build an {!module-type:Preface_specs.ARROW} using
    {!module-type:Preface_specs.Arrow.WITH_ARROW_AND_FST} over a
    {!module-type:Preface_specs.CATEGORY}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_category_and_via_arrow_and_fst
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow.WITH_ARROW_AND_FST
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Using arrow and split over a Category}

    Build an {!module-type:Preface_specs.ARROW} using
    {!module-type:Preface_specs.Arrow.WITH_ARROW_AND_SPLIT} over a
    {!module-type:Preface_specs.CATEGORY}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_category_and_via_arrow_an_split
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow.WITH_ARROW_AND_SPLIT
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) Req.t

(** {1 Arrow Algebra}

    Construction of {!module-type:Preface_specs.ARROW} by combining them. *)

(** {2 Product}

    Construct the product of two {!module-type:Preface_specs.ARROW}. *)

module Product (F : Preface_specs.ARROW) (G : Preface_specs.ARROW) :
  Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) F.t * ('a, 'b) G.t

(** {1 From other abstraction} *)

(** {2 From a Strong and a Category}

    Produces an {!module-type:Preface_specs.ARROW} from a
    {!module-type:Preface_specs.STRONG} and a
    {!module-type:Preface_specs.CATEGORY}.

    One way of looking at Arrow would be to see them as the conjunction of
    Category and Strong, as mentioned in {{:https://arxiv.org/pdf/1406.4823.pdf}
    Notions of Computation as Monoids} by E. Rivas and M. Jaskelioff. *)

module From_strong_and_category
    (Strong : Preface_specs.Strong.WITH_DIMAP_AND_FST)
    (Category : Preface_specs.CATEGORY with type ('a, 'b) t = ('a, 'b) Strong.t) :
  Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) Category.t

(** {2 From a Monad}

    Produces an {!module-type:Preface_specs.ARROW} from a
    {!module-type:Preface_specs.MONAD} (using the [Kleisli Arrow]). *)

module From_monad (Monad : Preface_specs.Monad.CORE) :
  Preface_specs.ARROW with type ('a, 'b) t = 'a -> 'b Monad.t

(** {2 From an Arrow Plus}

    Produces an {!module-type:Preface_specs.ARROW} from an
    {!module-type:Preface_specs.ARROW_PLUS}. *)

module From_arrow_plus (Plus : Preface_specs.ARROW_PLUS) :
  Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) Plus.t

(** {2 From an Arrow Alt}

    Produces an {!module-type:Preface_specs.ARROW} from an
    {!module-type:Preface_specs.ARROW_ALT}. *)

module From_arrow_alt (Alt : Preface_specs.ARROW_ALT) :
  Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) Alt.t

(** {2 From an Arrow Zero}

    Produces an {!module-type:Preface_specs.ARROW} from an
    {!module-type:Preface_specs.ARROW_ZERO}. *)

module From_arrow_zero (Zero : Preface_specs.ARROW_ZERO) :
  Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) Zero.t

(** {2 From an Arrow Choice}

    Produces an {!module-type:Preface_specs.ARROW} from an
    {!module-type:Preface_specs.ARROW_CHOICE}. *)

module From_arrow_choice (Choice : Preface_specs.ARROW_CHOICE) :
  Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) Choice.t

(** {2 From an Arrow Apply}

    Produces an {!module-type:Preface_specs.ARROW} from an
    {!module-type:Preface_specs.ARROW_APPLY}. *)

module From_arrow_apply (Apply : Preface_specs.ARROW_APPLY) :
  Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) Apply.t

(** {1 Manual construction}

    Advanced way to build an {!module-type:Preface_specs.ARROW}, constructing
    and assembling a component-by-component of
    {!module-type:Preface_specs.ARROW}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Arrow.CORE)
    (Operation : Preface_specs.Arrow.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t)
    (Alias : Preface_specs.Arrow.ALIAS
               with type ('a, 'b) t = ('a, 'b) Operation.t)
    (Infix : Preface_specs.Arrow.INFIX with type ('a, 'b) t = ('a, 'b) Alias.t) :
  Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) Infix.t

(** {2 Building Core} *)

module Core_over_category_and_via_arrow_and_fst
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow.WITH_ARROW_AND_FST
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow.CORE with type ('a, 'b) t = ('a, 'b) Req.t

module Core_over_category_and_via_arrow_and_split
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow.WITH_ARROW_AND_SPLIT
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow.CORE with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Deriving Operation} *)

module Operation_over_category
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow.CORE with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow.OPERATION with type ('a, 'b) t = ('a, 'b) Core.t

(** {2 Deriving Alias} *)

module Alias (Operation : Preface_specs.Arrow.OPERATION) :
  Preface_specs.Arrow.ALIAS with type ('a, 'b) t = ('a, 'b) Operation.t

(** {2 Deriving Infix} *)

module Infix_over_category
    (Category : Preface_specs.CATEGORY)
    (Core : Preface_specs.Arrow.CORE with type ('a, 'b) t = ('a, 'b) Category.t)
    (Operation : Preface_specs.Arrow.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) :
  Preface_specs.Arrow.INFIX with type ('a, 'b) t = ('a, 'b) Operation.t
