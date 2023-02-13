(** Building a {!module:Preface_specs.Indexed_alt} *)

(** {1 Using the minimal definition} *)

(** {2 Using neutral and combine}

    Build a {!module-type:Preface_specs.INDEXED_ALT} using
    {!module-type:Preface_specs.Indexed_alt.WITH_COMBINE_AND_MAP}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_map_and_combine
    (Req : Preface_specs.Indexed_alt.WITH_COMBINE_AND_MAP) :
  Preface_specs.INDEXED_ALT with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Over a Functor}

    Build a {!module-type:Preface_specs.INDEXED_ALT} over a
    {!module-type:Preface_specs.INDEXED_FUNCTOR}.

    If you already have a Functor, you can enrich it by passing only the
    [combine] function. *)

module Over_functor
    (Functor : Preface_specs.INDEXED_FUNCTOR)
    (Combine : Preface_specs.Indexed_alt.WITH_COMBINE
                 with type ('a, 'index) t = ('a, 'index) Functor.t) :
  Preface_specs.INDEXED_ALT with type ('a, 'index) t = ('a, 'index) Combine.t

(** {1 Manual construction}

    Advanced way to build an {!module-type:Preface_specs.INDEXED_ALT},
    constructing and assembling a component-by-component of
    {!module-type:Preface_specs.INDEXED_ALT}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Indexed_alt.CORE)
    (Operation : Preface_specs.Indexed_alt.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t)
    (Infix : Preface_specs.Indexed_alt.INFIX
               with type ('a, 'index) t = ('a, 'index) Operation.t)
    (Syntax : Preface_specs.Indexed_alt.SYNTAX
                with type ('a, 'index) t = ('a, 'index) Infix.t) :
  Preface_specs.INDEXED_ALT with type ('a, 'index) t = ('a, 'index) Infix.t

(** {2 Building Core} *)

module Core_over_functor
    (Functor : Preface_specs.INDEXED_FUNCTOR)
    (Req : Preface_specs.Indexed_alt.WITH_COMBINE
             with type ('a, 'index) t = ('a, 'index) Functor.t) :
  Preface_specs.Indexed_alt.CORE with type ('a, 'index) t = ('a, 'index) Req.t

module Core (Req : Preface_specs.Indexed_alt.WITH_COMBINE_AND_MAP) :
  Preface_specs.Indexed_alt.CORE with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Indexed_alt.CORE) :
  Preface_specs.Indexed_alt.OPERATION
    with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Deriving Infix} *)

module Infix
    (Core : Preface_specs.Indexed_alt.CORE)
    (Operation : Preface_specs.Indexed_alt.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t) :
  Preface_specs.Indexed_alt.INFIX with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Deriving Syntax} *)

module Syntax (Core : Preface_specs.Indexed_alt.CORE) :
  Preface_specs.Indexed_alt.SYNTAX
    with type ('a, 'index) t = ('a, 'index) Core.t
