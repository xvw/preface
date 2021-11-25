(** Building a {!module:Preface_specs.Arrow_choice} *)

(** {1 Using the minimal definition} *)

(** {2 Using left over an Arrow}

    Build an {!module-type:Preface_specs.ARROW_CHOICE} using
    {!module-type:Preface_specs.Arrow_choice.WITH_LEFT} over an
    {!module-type:Preface_specs.ARROW}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_arrow_with_left
    (Arrow : Preface_specs.ARROW)
    (Left : Preface_specs.Arrow_choice.WITH_LEFT
              with type ('a, 'b) t = ('a, 'b) Arrow.t) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) Left.t

(** {2 Using choose over an Arrow}

    Build an {!module-type:Preface_specs.ARROW_CHOICE} using
    {!module-type:Preface_specs.Arrow_choice.WITH_CHOOSE} over an
    {!module-type:Preface_specs.ARROW}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_arrow_with_choose
    (Arrow : Preface_specs.ARROW)
    (Choose : Preface_specs.Arrow_choice.WITH_CHOOSE
                with type ('a, 'b) t = ('a, 'b) Arrow.t) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) Choose.t

(** {2 Using left and choose over an Arrow}

    Build an {!module-type:Preface_specs.ARROW_CHOICE} using
    {!module-type:Preface_specs.Arrow_choice.WITH_LEFT_AND_CHOOSE} over an
    {!module-type:Preface_specs.ARROW}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_arrow_with_left_and_choose
    (Arrow : Preface_specs.ARROW)
    (Choose_left : Preface_specs.Arrow_choice.WITH_LEFT_AND_CHOOSE
                     with type ('a, 'b) t = ('a, 'b) Arrow.t) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) Choose_left.t

(** {2 Using left, arrow and fst over a Category}

    Build an {!module-type:Preface_specs.ARROW_CHOICE} using
    {!module-type:Preface_specs.Arrow_choice.WITH_ARROW_AND_FST_AND_LEFT} over a
    {!module-type:Preface_specs.CATEGORY}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_category_and_via_arrow_and_fst_and_left
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_choice.WITH_ARROW_AND_FST_AND_LEFT
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Using choose, arrow and fst over a Category}

    Build an {!module-type:Preface_specs.ARROW_CHOICE} using
    {!module-type:Preface_specs.Arrow_choice.WITH_ARROW_AND_FST_AND_CHOOSE} over
    a {!module-type:Preface_specs.CATEGORY}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_category_and_via_arrow_and_fst_and_choose
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_choice.WITH_ARROW_AND_FST_AND_CHOOSE
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Using left, arrow and split over a Category}

    Build an {!module-type:Preface_specs.ARROW_CHOICE} using
    {!module-type:Preface_specs.Arrow_choice.WITH_ARROW_AND_SPLIT_AND_LEFT} over
    a {!module-type:Preface_specs.CATEGORY}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_over_category_and_via_arrow_and_split_and_left
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_choice.WITH_ARROW_AND_SPLIT_AND_LEFT
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Using choose, arrow and split over a Category}

    Build an {!module-type:Preface_specs.ARROW_CHOICE} using
    {!module-type:Preface_specs.Arrow_choice.WITH_ARROW_AND_SPLIT_AND_CHOOSE}
    over a {!module-type:Preface_specs.CATEGORY}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_category_and_via_arrow_and_split_and_choose
    (Category : Preface_specs.CATEGORY)
    (Req : Preface_specs.Arrow_choice.WITH_ARROW_AND_SPLIT_AND_CHOOSE
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) Req.t

(** {1 Arrow Choice Algebra}

    Construction of {!module-type:Preface_specs.ARROW_CHOICE} by combining them. *)

(** {2 Product}

    Construct the product of two {!module-type:Preface_specs.ARROW_CHOICE}. *)

module Product (F : Preface_specs.ARROW_CHOICE) (G : Preface_specs.ARROW_CHOICE) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) F.t * ('a, 'b) G.t

(** {1 From other abstraction} *)

(** {2 From a Monad}

    Produces an {!module-type:Preface_specs.ARROW_CHOICE} from a
    {!module-type:Preface_specs.MONAD} (using the [Kleisli Arrow]). *)

module From_monad (Monad : Preface_specs.Monad.CORE) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = 'a -> 'b Monad.t

(** {1 Manual construction}

    Advanced way to build an {!module-type:Preface_specs.ARROW_CHOICE},
    constructing and assembling a component-by-component of
    {!module-type:Preface_specs.ARROW_CHOICE}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Arrow_choice.CORE)
    (Operation : Preface_specs.Arrow_choice.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t)
    (Alias : Preface_specs.Arrow_choice.ALIAS
               with type ('a, 'b) t = ('a, 'b) Operation.t)
    (Infix : Preface_specs.Arrow_choice.INFIX
               with type ('a, 'b) t = ('a, 'b) Alias.t) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) Infix.t

(** {2 Building Core} *)

module Core_over_category_and_via_arrow_and_fst_and_left
    (Category : Preface_specs.Category.CORE)
    (Req : Preface_specs.Arrow_choice.WITH_ARROW_AND_FST_AND_LEFT
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_choice.CORE with type ('a, 'b) t = ('a, 'b) Req.t

module Core_over_category_and_via_arrow_and_split_and_left
    (Category : Preface_specs.Category.CORE)
    (Req : Preface_specs.Arrow_choice.WITH_ARROW_AND_SPLIT_AND_LEFT
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_choice.CORE with type ('a, 'b) t = ('a, 'b) Req.t

module Core_over_category_and_via_arrow_and_fst_and_choose
    (Category : Preface_specs.Category.CORE)
    (Req : Preface_specs.Arrow_choice.WITH_ARROW_AND_FST_AND_CHOOSE
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_choice.CORE with type ('a, 'b) t = ('a, 'b) Req.t

module Core_over_category_and_via_arrow_and_split_and_choose
    (Category : Preface_specs.Category.CORE)
    (Req : Preface_specs.Arrow_choice.WITH_ARROW_AND_SPLIT_AND_CHOOSE
             with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_choice.CORE with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Deriving Operation} *)

module Operation_over_category
    (Category : Preface_specs.Category.OPERATION)
    (Core : Preface_specs.Arrow_choice.CORE
              with type ('a, 'b) t = ('a, 'b) Category.t) :
  Preface_specs.Arrow_choice.OPERATION with type ('a, 'b) t = ('a, 'b) Core.t

(** {2 Deriving Alias} *)

module Alias (Operation : Preface_specs.Arrow.OPERATION) :
  Preface_specs.Arrow.ALIAS with type ('a, 'b) t = ('a, 'b) Operation.t

(** {2 Deriving Infix} *)

module Infix_over_category
    (Category : Preface_specs.Category.INFIX)
    (Core : Preface_specs.Arrow_choice.CORE
              with type ('a, 'b) t = ('a, 'b) Category.t)
    (Operation : Preface_specs.Arrow_choice.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) :
  Preface_specs.Arrow_choice.INFIX with type ('a, 'b) t = ('a, 'b) Core.t
