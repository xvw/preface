(** Building a {!module:Preface_specs.Indexed_functor} *)

(** {1 Using the minimal definition}

    Build a {!module-type:Preface_specs.INDEXED_FUNCTOR} using
    {!module-type:Preface_specs.Indexed_functor.WITH_MAP}.

    Standard method, using the minimal definition of a functor to derive its
    full API. *)

module Via_map (Req : Preface_specs.Indexed_functor.WITH_MAP) :
  Preface_specs.INDEXED_FUNCTOR with type ('a, 'index) t = ('a, 'index) Req.t

(** {1 Manual construction}

    Advanced way to build a {!module-type:Preface_specs.INDEXED_FUNCTOR},
    constructing and assembling a component-by-component of
    {!module-type:Preface_specs.INDEXED_FUNCTOR}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Indexed_functor.CORE)
    (Operation : Preface_specs.Indexed_functor.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t)
    (Infix : Preface_specs.Indexed_functor.INFIX
               with type ('a, 'index) t = ('a, 'index) Core.t)
    (Syntax : Preface_specs.Indexed_functor.SYNTAX
                with type ('a, 'index) t = ('a, 'index) Core.t) :
  Preface_specs.INDEXED_FUNCTOR with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Building Core} *)

module Core (Req : Preface_specs.Indexed_functor.WITH_MAP) :
  Preface_specs.Indexed_functor.CORE
    with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Indexed_functor.CORE) :
  Preface_specs.Indexed_functor.OPERATION
    with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Deriving Infix} *)

module Infix
    (Core : Preface_specs.Indexed_functor.CORE)
    (Operation : Preface_specs.Indexed_functor.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t) :
  Preface_specs.Indexed_functor.INFIX
    with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Deriving Syntax} *)

module Syntax (Core : Preface_specs.Indexed_functor.CORE) :
  Preface_specs.Indexed_functor.SYNTAX
    with type ('a, 'index) t = ('a, 'index) Core.t
