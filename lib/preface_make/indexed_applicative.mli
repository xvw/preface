(** Building a {!module:Preface_specs.Indexed_applicative} *)

(** {1 Using the minimal definition} *)

(** {2 Using pure and apply}

    Build a {!module-type:Preface_specs.INDEXED_APPLICATIVE} using
    {!module-type:Preface_specs.Indexed_applicative.WITH_APPLY}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_pure_and_apply
    (Req : Preface_specs.Indexed_applicative.WITH_PURE_AND_APPLY) :
  Preface_specs.INDEXED_APPLICATIVE
    with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Using pure, map and product}

    Build a {!module-type:Preface_specs.INDEXED_APPLICATIVE} using
    {!module-type:Preface_specs.Indexed_applicative.WITH_MAP_AND_PRODUCT}.

    Other standard method, using the minimal definition of an alt to derive its
    full API. *)

module Via_pure_map_and_product
    (Req : Preface_specs.Indexed_applicative.WITH_PURE_MAP_AND_PRODUCT) :
  Preface_specs.INDEXED_APPLICATIVE
    with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Using pure and lift2}

    Build a {!module-type:Preface_specs.INDEXED_APPLICATIVE} using
    {!module-type:Preface_specs.Indexed_applicative.WITH_LIFT2}.

    Other standard method, using the minimal definition of an alt to derive its
    full API. *)

module Via_pure_and_lift2
    (Req : Preface_specs.Indexed_applicative.WITH_PURE_AND_LIFT2) :
  Preface_specs.INDEXED_APPLICATIVE
    with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Over an apply}

    Build a {!module-type:Preface_specs.INDEXED_APPLICATIVE} over an
    {!module-type:Preface_specs.INDEXED_APPLY}.

    If you already have an Apply, you can enrich it by passing only [pure] *)

module Over_apply
    (Apply : Preface_specs.INDEXED_APPLY)
    (Req : Preface_specs.Indexed_applicative.WITH_PURE
             with type ('a, 'index) t = ('a, 'index) Apply.t) :
  Preface_specs.INDEXED_APPLICATIVE
    with type ('a, 'index) t = ('a, 'index) Req.t

(** {1 Manual construction}

    Advanced way to build an {!module-type:Preface_specs.INDEXED_APPLICATIVE},
    constructing and assembling a component-by-component of
    {!module-type:Preface_specs.INDEXED_APPLICATIVE}. (In order to provide your
    own implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Indexed_applicative.CORE)
    (Operation : Preface_specs.Indexed_applicative.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t)
    (Infix : Preface_specs.Indexed_applicative.INFIX
               with type ('a, 'index) t = ('a, 'index) Core.t)
    (Syntax : Preface_specs.Indexed_applicative.SYNTAX
                with type ('a, 'index) t = ('a, 'index) Core.t) :
  Preface_specs.INDEXED_APPLICATIVE
    with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Building Core} *)

module Core_via_pure_map_and_product
    (Req : Preface_specs.Indexed_applicative.WITH_PURE_MAP_AND_PRODUCT) :
  Preface_specs.Indexed_applicative.CORE
    with type ('a, 'index) t = ('a, 'index) Req.t

module Core_via_pure_and_apply
    (Req : Preface_specs.Indexed_applicative.WITH_PURE_AND_APPLY) :
  Preface_specs.Indexed_applicative.CORE
    with type ('a, 'index) t = ('a, 'index) Req.t

module Core_via_pure_and_lift2
    (Req : Preface_specs.Indexed_applicative.WITH_PURE_AND_LIFT2) :
  Preface_specs.Indexed_applicative.CORE
    with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Indexed_applicative.CORE) :
  Preface_specs.Indexed_applicative.OPERATION
    with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Deriving Syntax} *)

module Syntax (Core : Preface_specs.Indexed_applicative.CORE) :
  Preface_specs.Indexed_applicative.SYNTAX
    with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Deriving Infix} *)

module Infix
    (Core : Preface_specs.Indexed_applicative.CORE)
    (Operation : Preface_specs.Indexed_applicative.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t) :
  Preface_specs.Indexed_applicative.INFIX
    with type ('a, 'index) t = ('a, 'index) Core.t
