(** Building a {!module:Preface_specs.Comonad} *)

(** {1 Using the minimal definition} *)

(** {2 Using extract and extend}

    Build a {!module-type:Preface_specs.COMONAD} using
    {!module-type:Preface_specs.Comonad.WITH_EXTEND}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_extend (Req : Preface_specs.Comonad.WITH_EXTEND) :
  Preface_specs.COMONAD with type 'a t = 'a Req.t

(** {2 Using extract, map and duplicate}

    Build a {!module-type:Preface_specs.COMONAD} using
    {!module-type:Preface_specs.Comonad.WITH_MAP_AND_DUPLICATE}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_map_and_duplicate
    (Req : Preface_specs.Comonad.WITH_MAP_AND_DUPLICATE) :
  Preface_specs.COMONAD with type 'a t = 'a Req.t

(** {2 Using extract and the cokleisli composition}

    Build a {!module-type:Preface_specs.COMONAD} using
    {!module-type:Preface_specs.Comonad.WITH_COKLEISLI_COMPOSITION}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_cokleisli_composition
    (Req : Preface_specs.Comonad.WITH_COKLEISLI_COMPOSITION) :
  Preface_specs.COMONAD with type 'a t = 'a Req.t

(** {1 To other abstraction} *)

(** {2 To an Indexed Comonad} *)

module Index (F : Preface_specs.COMONAD) :
  Preface_specs.INDEXED_COMONAD with type ('a, 'index) t = 'a F.t

(** {1 Manual construction}

    Advanced way to build a {!module-type:Preface_specs.COMONAD}, constructing
    and assembling a component-by-component of
    {!module-type:Preface_specs.COMONAD}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Comonad.CORE)
    (Operation : Preface_specs.Comonad.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Comonad.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Comonad.SYNTAX with type 'a t = 'a Core.t) :
  Preface_specs.COMONAD with type 'a t = 'a Core.t

(** {2 Building Core} *)

module Core_via_map_and_duplicate
    (Req : Preface_specs.Comonad.WITH_MAP_AND_DUPLICATE) :
  Preface_specs.Comonad.CORE with type 'a t = 'a Req.t

module Core_via_extend (Req : Preface_specs.Comonad.WITH_EXTEND) :
  Preface_specs.Comonad.CORE with type 'a t = 'a Req.t

module Core_via_cokleisli_composition
    (Req : Preface_specs.Comonad.WITH_COKLEISLI_COMPOSITION) :
  Preface_specs.Comonad.CORE with type 'a t = 'a Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Comonad.CORE) :
  Preface_specs.Comonad.OPERATION with type 'a t = 'a Core.t

(** {2 Deriving Syntax} *)

module Syntax (Core : Preface_specs.Comonad.CORE) :
  Preface_specs.Comonad.SYNTAX with type 'a t = 'a Core.t

(** {2 Deriving Infix} *)

module Infix
    (Core : Preface_specs.Comonad.CORE)
    (Operation : Preface_specs.Comonad.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Comonad.INFIX with type 'a t = 'a Core.t
