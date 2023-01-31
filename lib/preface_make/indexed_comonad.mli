(** Building an {!module:Preface_specs.Indexed_comonad} *)

(** {1 Using the minimal definition} *)

(** {2 Using extract and extend}

    Build a {!module-type:Preface_specs.INDEXED_COMONAD} using
    {!module-type:Preface_specs.Indexed_comonad.WITH_EXTEND}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_extend (Req : Preface_specs.Indexed_comonad.WITH_EXTEND) :
  Preface_specs.INDEXED_COMONAD with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Using extract, map and duplicate}

    Build a {!module-type:Preface_specs.INDEXED_COMONAD} using
    {!module-type:Preface_specs.Indexed_comonad.WITH_MAP_AND_DUPLICATE}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_map_and_duplicate
    (Req : Preface_specs.Indexed_comonad.WITH_MAP_AND_DUPLICATE) :
  Preface_specs.INDEXED_COMONAD with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Using extract and the cokleisli composition}

    Build a {!module-type:Preface_specs.INDEXED_COMONAD} using
    {!module-type:Preface_specs.Indexed_comonad.WITH_COKLEISLI_COMPOSITION}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_cokleisli_composition
    (Req : Preface_specs.Indexed_comonad.WITH_COKLEISLI_COMPOSITION) :
  Preface_specs.INDEXED_COMONAD with type ('a, 'index) t = ('a, 'index) Req.t

(** {1 Manual construction}

    Advanced way to build a {!module-type:Preface_specs.INDEXED_COMONAD},
    constructing and assembling a component-by-component of
    {!module-type:Preface_specs.INDEXED_COMONAD}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Indexed_comonad.CORE)
    (Operation : Preface_specs.Indexed_comonad.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t)
    (Infix : Preface_specs.Indexed_comonad.INFIX
               with type ('a, 'index) t = ('a, 'index) Core.t)
    (Syntax : Preface_specs.Indexed_comonad.SYNTAX
                with type ('a, 'index) t = ('a, 'index) Core.t) :
  Preface_specs.INDEXED_COMONAD with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Building Core} *)

module Core_via_map_and_duplicate
    (Req : Preface_specs.Indexed_comonad.WITH_MAP_AND_DUPLICATE) :
  Preface_specs.Indexed_comonad.CORE
    with type ('a, 'index) t = ('a, 'index) Req.t

module Core_via_extend (Req : Preface_specs.Indexed_comonad.WITH_EXTEND) :
  Preface_specs.Indexed_comonad.CORE
    with type ('a, 'index) t = ('a, 'index) Req.t

module Core_via_cokleisli_composition
    (Req : Preface_specs.Indexed_comonad.WITH_COKLEISLI_COMPOSITION) :
  Preface_specs.Indexed_comonad.CORE
    with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Indexed_comonad.CORE) :
  Preface_specs.Indexed_comonad.OPERATION
    with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Deriving Syntax} *)

module Syntax (Core : Preface_specs.Indexed_comonad.CORE) :
  Preface_specs.Indexed_comonad.SYNTAX
    with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Deriving Infix} *)

module Infix
    (Core : Preface_specs.Indexed_comonad.CORE)
    (Operation : Preface_specs.Indexed_comonad.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t) :
  Preface_specs.Indexed_comonad.INFIX
    with type ('a, 'index) t = ('a, 'index) Core.t
