(** Building a {!module:Preface_specs.INDEXED_APPLY} *)

(** {1 Using the minimal definition} *)

(** {2 Using map and apply}

    Build a {!module-type:Preface_specs.APPLY} using
    {!module-type:Preface_specs.APPLY.WITH_MAP_AND_APPLY}.

    Other standard method, using the minimal definition of an alt to derive its
    full API. *)

(** {2 Using apply over functor}

    Build a {!module-type:Preface_specs.INDEXED_APPLY} using
    {!module-type:Preface_specs.Indexed_functor.WITH_MAP} and
    {!module-type:Preface_specs.Indexed_apply.WITH_APPLY}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)
module Via_map_and_apply (Req : Preface_specs.Indexed_apply.WITH_MAP_AND_APPLY) :
  Preface_specs.INDEXED_APPLY with type ('a, 'index) t = ('a, 'index) Req.t

module Over_functor_via_apply
    (Functor : Preface_specs.Indexed_functor.WITH_MAP)
    (Req : Preface_specs.Indexed_apply.WITH_APPLY
             with type ('a, 'index) t = ('a, 'index) Functor.t) :
  Preface_specs.INDEXED_APPLY with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Using map and product}

    Build a {!module-type:Preface_specs.INDEXED_APPLY} using
    {!module-type:Preface_specs.Indexed_apply.WITH_MAP_AND_PRODUCT}.

    Other standard method, using the minimal definition of an alt to derive its
    full API. *)

module Via_map_and_product
    (Req : Preface_specs.Indexed_apply.WITH_MAP_AND_PRODUCT) :
  Preface_specs.INDEXED_APPLY with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Using product over functor}

    Build a {!module-type:Preface_specs.INDEXED_APPLY} using
    {!module-type:Preface_specs.Indexed_functor.WITH_MAP} and
    {!module-type:Preface_specs.Indexed_apply.WITH_PRODUCT}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_functor_via_product
    (Functor : Preface_specs.Indexed_functor.WITH_MAP)
    (Req : Preface_specs.Indexed_apply.WITH_PRODUCT
             with type ('a, 'index) t = ('a, 'index) Functor.t) :
  Preface_specs.INDEXED_APPLY with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Using map and lift2}

    Build a {!module-type:Preface_specs.INDEXED_APPLY} using
    {!module-type:Preface_specs.Indexed_apply.WITH_LIFT2}.

    Other standard method, using the minimal definition of an alt to derive its
    full API. *)

module Via_map_and_lift2 (Req : Preface_specs.Indexed_apply.WITH_MAP_AND_LIFT2) :
  Preface_specs.INDEXED_APPLY with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Using lift2 over functor}

    Build a {!module-type:Preface_specs.INDEXED_APPLY} using
    {!module-type:Preface_specs.Indexed_functor.WITH_MAP} and
    {!module-type:Preface_specs.Indexed_apply.WITH_LIFT2}.

    Other standard method, using the minimal definition of an alt to derive its
    full API. *)

module Over_functor_via_lift2
    (Functor : Preface_specs.Indexed_functor.WITH_MAP)
    (Req : Preface_specs.Indexed_apply.WITH_LIFT2
             with type ('a, 'index) t = ('a, 'index) Functor.t) :
  Preface_specs.INDEXED_APPLY with type ('a, 'index) t = ('a, 'index) Req.t

(** {1 Manual construction}

    Advanced way to build an {!module-type:Preface_specs.INDEXED_APPLY},
    constructing and assembling a component-by-component of
    {!module-type:Preface_specs.INDEXED_APPLY}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Indexed_apply.CORE)
    (Operation : Preface_specs.Indexed_apply.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t)
    (Infix : Preface_specs.Indexed_apply.INFIX
               with type ('a, 'index) t = ('a, 'index) Core.t)
    (Syntax : Preface_specs.Indexed_apply.SYNTAX
                with type ('a, 'index) t = ('a, 'index) Core.t) :
  Preface_specs.INDEXED_APPLY with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Building Core} *)

module Core_via_map_and_apply
    (Req : Preface_specs.Indexed_apply.WITH_MAP_AND_APPLY) :
  Preface_specs.Indexed_apply.CORE with type ('a, 'index) t = ('a, 'index) Req.t

module Core_via_map_and_product
    (Req : Preface_specs.Indexed_apply.WITH_MAP_AND_PRODUCT) :
  Preface_specs.Indexed_apply.CORE with type ('a, 'index) t = ('a, 'index) Req.t

module Core_via_map_and_lift2
    (Req : Preface_specs.Indexed_apply.WITH_MAP_AND_LIFT2) :
  Preface_specs.Indexed_apply.CORE with type ('a, 'index) t = ('a, 'index) Req.t

module Core_over_functor_via_apply
    (Functor : Preface_specs.Indexed_functor.WITH_MAP)
    (Req : Preface_specs.Indexed_apply.WITH_APPLY
             with type ('a, 'index) t = ('a, 'index) Functor.t) :
  Preface_specs.Indexed_apply.CORE with type ('a, 'index) t = ('a, 'index) Req.t

module Core_over_functor_via_product
    (Functor : Preface_specs.Indexed_functor.WITH_MAP)
    (Req : Preface_specs.Indexed_apply.WITH_PRODUCT
             with type ('a, 'index) t = ('a, 'index) Functor.t) :
  Preface_specs.Indexed_apply.CORE with type ('a, 'index) t = ('a, 'index) Req.t

module Core_over_functor_via_lift2
    (Functor : Preface_specs.Indexed_functor.WITH_MAP)
    (Req : Preface_specs.Indexed_apply.WITH_LIFT2
             with type ('a, 'index) t = ('a, 'index) Functor.t) :
  Preface_specs.Indexed_apply.CORE with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Indexed_apply.CORE) :
  Preface_specs.Indexed_apply.OPERATION
    with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Deriving Syntax} *)

module Syntax (Core : Preface_specs.Indexed_apply.CORE) :
  Preface_specs.Indexed_apply.SYNTAX
    with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Deriving Infix} *)

module Infix
    (Core : Preface_specs.Indexed_apply.CORE)
    (Operation : Preface_specs.Indexed_apply.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t) :
  Preface_specs.Indexed_apply.INFIX
    with type ('a, 'index) t = ('a, 'index) Core.t
