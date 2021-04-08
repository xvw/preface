(** Building a {!module:Preface_specs.Alternative} *)

(** {1 Using the minimal definition} *)

(** {2 Using pure, apply, neutral and combine}

    Build a {!module-type:Preface_specs.ALTERNATIVE} using
    {!module-type:Preface_specs.Alternative.WITH_APPLY}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_apply (Req : Preface_specs.Alternative.WITH_APPLY) :
  Preface_specs.ALTERNATIVE with type 'a t = 'a Req.t

(** {2 Using pure, map, product, neutral and combine}

    Build a {!module-type:Preface_specs.ALTERNATIVE} using
    {!module-type:Preface_specs.Alternative.WITH_MAP_AND_PRODUCT}.

    Other standard method, using the minimal definition of an alt to derive its
    full API. *)

module Via_map_and_product
    (Req : Preface_specs.Alternative.WITH_MAP_AND_PRODUCT) :
  Preface_specs.ALTERNATIVE with type 'a t = 'a Req.t

(** {2 Over an applicative}

    Build a {!module-type:Preface_specs.ALTERNATIVE} over an
    {!module-type:Preface_specs.APPLICATIVE}.

    If you already have an Applicative, you can enrich it by passing only
    [combine] and [neutral].. *)

module Over_applicative
    (Applicative : Preface_specs.APPLICATIVE)
    (Req : Preface_specs.Alternative.WITH_NEUTRAL_AND_COMBINE
             with type 'a t = 'a Applicative.t) :
  Preface_specs.ALTERNATIVE with type 'a t = 'a Req.t

(** {1 Alternative Algebra}

    Construction of {!module-type:Preface_specs.ALTERNATIVE} by combining them. *)

(** {2 Composition}

    Right-to-left composition of {!module-type:Preface_specs.ALTERNATIVE} with
    {!module-type:Preface_specs.APPLICATIVE}.*)

module Composition
    (F : Preface_specs.ALTERNATIVE)
    (G : Preface_specs.APPLICATIVE) :
  Preface_specs.ALTERNATIVE with type 'a t = 'a G.t F.t

(** {2 Product}

    Construct the product of two {!module-type:Preface_specs.ALTERNATIVE}. *)

module Product (F : Preface_specs.ALTERNATIVE) (G : Preface_specs.ALTERNATIVE) :
  Preface_specs.ALTERNATIVE with type 'a t = 'a F.t * 'a G.t

(** {1 From other abstraction} *)

(** {2 From an Arrow Plus}

    Produces an {!module-type:Preface_specs.ALTERNATIVE} from an
    {!module-type:Preface_specs.ARROW_PLUS}. *)

module From_arrow_plus (A : Preface_specs.ARROW_PLUS) :
  Preface_specs.ALTERNATIVE with type 'a t = (unit, 'a) A.t

(** {1 Manual construction}

    Advanced way to build an {!module-type:Preface_specs.ALTERNATIVE},
    constructing and assembling a component-by-component of
    {!module-type:Preface_specs.ALTERNATIVE}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Alternative.CORE)
    (Operation : Preface_specs.Alternative.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Alternative.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Alternative.SYNTAX with type 'a t = 'a Core.t) :
  Preface_specs.ALTERNATIVE with type 'a t = 'a Core.t

(** {2 Building Core} *)

module Core_via_map_and_product
    (Req : Preface_specs.Alternative.WITH_MAP_AND_PRODUCT) :
  Preface_specs.Alternative.CORE with type 'a t = 'a Req.t

module Core_via_apply (Req : Preface_specs.Alternative.WITH_APPLY) :
  Preface_specs.Alternative.CORE with type 'a t = 'a Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Alternative.CORE) :
  Preface_specs.Alternative.OPERATION with type 'a t = 'a Core.t

(** {2 Deriving Syntax} *)

module Syntax (Core : Preface_specs.Alternative.CORE) :
  Preface_specs.Alternative.SYNTAX with type 'a t = 'a Core.t

(** {2 Deriving Infix} *)

module Infix
    (Core : Preface_specs.Alternative.CORE)
    (Operation : Preface_specs.Alternative.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Alternative.INFIX with type 'a t = 'a Core.t
