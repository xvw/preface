(** Building a {!module:Preface_specs.Indexed_alternative} *)

(** {1 Using the minimal definition} *)

(** {2 Using pure, apply, neutral and combine}

    Build a {!module-type:Preface_specs.INDEXED_ALTERNATIVE} using
    {!module-type:Preface_specs.Indexed_alternative.WITH_APPLY}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_pure_and_apply
    (Req : Preface_specs.Indexed_alternative.WITH_PURE_AND_APPLY) :
  Preface_specs.INDEXED_ALTERNATIVE
    with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Using pure, map, product, neutral and combine}

    Build a {!module-type:Preface_specs.INDEXED_ALTERNATIVE} using
    {!module-type:Preface_specs.Indexed_alternative.WITH_MAP_AND_PRODUCT}.

    Other standard method, using the minimal definition of an alt to derive its
    full API. *)

module Via_pure_map_and_product
    (Req : Preface_specs.Indexed_alternative.WITH_PURE_MAP_AND_PRODUCT) :
  Preface_specs.INDEXED_ALTERNATIVE
    with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Using pure, lift2, neutral and combine}

    Build a {!module-type:Preface_specs.INDEXED_ALTERNATIVE} using
    {!module-type:Preface_specs.Indexed_alternative.WITH_LIFT2}.

    Other standard method, using the minimal definition of an alt to derive its
    full API. *)

module Via_pure_and_lift2
    (Req : Preface_specs.Indexed_alternative.WITH_PURE_AND_LIFT2) :
  Preface_specs.INDEXED_ALTERNATIVE
    with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Over an applicative}

    Build a {!module-type:Preface_specs.INDEXED_ALTERNATIVE} over an
    {!module-type:Preface_specs.INDEXED_APPLICATIVE}.

    If you already have an Applicative, you can enrich it by passing only
    [combine] and [neutral]. *)

module Over_applicative
    (Applicative : Preface_specs.INDEXED_APPLICATIVE)
    (Req : Preface_specs.Indexed_alternative.WITH_NEUTRAL_AND_COMBINE
             with type ('a, 'index) t = ('a, 'index) Applicative.t) :
  Preface_specs.INDEXED_ALTERNATIVE
    with type ('a, 'index) t = ('a, 'index) Req.t

(** {1 Manual construction}

    Advanced way to build an {!module-type:Preface_specs.INDEXED_ALTERNATIVE},
    constructing and assembling a component-by-component of
    {!module-type:Preface_specs.INDEXED_ALTERNATIVE}. (In order to provide your
    own implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Indexed_alternative.CORE)
    (Operation : Preface_specs.Indexed_alternative.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t)
    (Infix : Preface_specs.Indexed_alternative.INFIX
               with type ('a, 'index) t = ('a, 'index) Core.t)
    (Syntax : Preface_specs.Indexed_alternative.SYNTAX
                with type ('a, 'index) t = ('a, 'index) Core.t) :
  Preface_specs.INDEXED_ALTERNATIVE
    with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Building Core} *)

module Core_via_pure_map_and_product
    (Req : Preface_specs.Indexed_alternative.WITH_PURE_MAP_AND_PRODUCT) :
  Preface_specs.Indexed_alternative.CORE
    with type ('a, 'index) t = ('a, 'index) Req.t

module Core_via_pure_and_apply
    (Req : Preface_specs.Indexed_alternative.WITH_PURE_AND_APPLY) :
  Preface_specs.Indexed_alternative.CORE
    with type ('a, 'index) t = ('a, 'index) Req.t

module Core_via_pure_and_lift2
    (Req : Preface_specs.Indexed_alternative.WITH_PURE_AND_LIFT2) :
  Preface_specs.Indexed_alternative.CORE
    with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Indexed_alternative.CORE) :
  Preface_specs.Indexed_alternative.OPERATION
    with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Deriving Syntax} *)

module Syntax (Core : Preface_specs.Indexed_alternative.CORE) :
  Preface_specs.Indexed_alternative.SYNTAX
    with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Deriving Infix} *)

module Infix
    (Core : Preface_specs.Indexed_alternative.CORE)
    (Operation : Preface_specs.Indexed_alternative.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t) :
  Preface_specs.Indexed_alternative.INFIX
    with type ('a, 'index) t = ('a, 'index) Core.t
