(** Building a {!module:Preface_specs.Alt} *)

(** {1 Using the minimal definition} *)

(** {2 Using neutral and combine}

    Build a {!module-type:Preface_specs.ALT} using
    {!module-type:Preface_specs.Alt.WITH_COMBINE_AND_MAP}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_map_and_combine (Req : Preface_specs.Alt.WITH_COMBINE_AND_MAP) :
  Preface_specs.ALT with type 'a t = 'a Req.t

(** {2 Over a Functor}

    Build a {!module-type:Preface_specs.ALT} over a
    {!module-type:Preface_specs.FUNCTOR}.

    If you already have a Functor, you can enrich it by passing only the
    [combine] function. *)

module Over_functor
    (Functor : Preface_specs.FUNCTOR)
    (Combine : Preface_specs.Alt.WITH_COMBINE with type 'a t = 'a Functor.t) :
  Preface_specs.ALT with type 'a t = 'a Combine.t

(** {1 Functor Algebra}

    Construction of {!module-type:Preface_specs.ALT} by combining them. *)

(** {2 Composition}

    Right-to-left composition of Alt with {!module-type:Preface_specs.FUNCTOR}. *)

module Composition (F : Preface_specs.ALT) (G : Preface_specs.FUNCTOR) :
  Preface_specs.ALT with type 'a t = 'a G.t F.t

(** {2 Product}

    Construct the product of two {!module-type:Preface_specs.ALT}. *)

module Product (F : Preface_specs.ALT) (G : Preface_specs.ALT) :
  Preface_specs.ALT with type 'a t = 'a F.t * 'a G.t

(** {1 Manual construction}

    Advanced way to build an {!module-type:Preface_specs.ALT}, constructing and
    assembling a component-by-component of {!module-type:Preface_specs.ALT}. (In
    order to provide your own implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Alt.CORE)
    (Operation : Preface_specs.Alt.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Alt.INFIX with type 'a t = 'a Operation.t) :
  Preface_specs.ALT with type 'a t = 'a Infix.t

(** {2 Building Core} *)

module Core_over_functor
    (Functor : Preface_specs.FUNCTOR)
    (Req : Preface_specs.Alt.WITH_COMBINE with type 'a t = 'a Functor.t) :
  Preface_specs.Alt.CORE with type 'a t = 'a Req.t

module Core (Req : Preface_specs.Alt.WITH_COMBINE_AND_MAP) :
  Preface_specs.Alt.CORE with type 'a t = 'a Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Alt.CORE) :
  Preface_specs.Alt.OPERATION with type 'a t = 'a Core.t

(** {2 Deriving Infix} *)

module Infix
    (Core : Preface_specs.Alt.CORE)
    (Operation : Preface_specs.Alt.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Alt.INFIX with type 'a t = 'a Core.t
