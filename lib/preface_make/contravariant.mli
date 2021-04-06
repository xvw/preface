(** Building a {!module:Preface_specs.Contravariant} *)

(** {1 Using the minimal definition}

    Build a {!module-type:Preface_specs.CONTRAVARIANT} using
    {!module-type:Preface_specs.Contravariant.WITH_CONTRAMAP}.

    Standard method, using the minimal definition of a contravariant functor to
    derive its full API. *)

module Via_contramap (Req : Preface_specs.Contravariant.WITH_CONTRAMAP) :
  Preface_specs.CONTRAVARIANT with type 'a t = 'a Req.t

(** {1 Contravariant Algebra}

    Construction of {!module-type:Preface_specs.CONTRAVARIANT} by combining
    them. *)

(** {2 Contravariant composition}

    Construction of {!module-type:Preface_specs.CONTRAVARIANT} by left-to-right
    composition with {!module-type:Preface_specs.FUNCTOR}. *)

module Composition (F : Preface_specs.FUNCTOR) (G : Preface_specs.CONTRAVARIANT) :
  Preface_specs.CONTRAVARIANT with type 'a t = 'a G.t F.t

(** {1 Manual construction}

    Advanced way to build a {!module-type:Preface_specs.CONTRAVARIANT},
    constructing and assembling a component-by-component of
    {!module-type:Preface_specs.CONTRAVARIANT}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Contravariant.CORE)
    (Operation : Preface_specs.Contravariant.OPERATION
                   with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Contravariant.INFIX with type 'a t = 'a Operation.t) :
  Preface_specs.CONTRAVARIANT with type 'a t = 'a Infix.t

(** {2 Building Core} *)

module Core (Req : Preface_specs.Contravariant.WITH_CONTRAMAP) :
  Preface_specs.Contravariant.CORE with type 'a t = 'a Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Contravariant.CORE) :
  Preface_specs.Contravariant.OPERATION with type 'a t = 'a Core.t

(** {2 Deriving Infix} *)

module Infix
    (Core : Preface_specs.Contravariant.CORE)
    (Operation : Preface_specs.Contravariant.OPERATION
                   with type 'a t = 'a Core.t) :
  Preface_specs.Contravariant.INFIX with type 'a t = 'a Operation.t
