(** Building a {!module:Preface_specs.Meet_semilattice} *)

(** {1 Using the minimal definition}

    Build a {!module-type:Preface_specs.MEET_SEMILATTICE} using
    {!module-type:Preface_specs.Meet_semilattice.WITH_MEET}.

    Standard method, using the minimal definition of a semigroup to derive its
    full API. *)

module Via_meet (Req : Preface_specs.Meet_semilattice.WITH_MEET) :
  Preface_specs.MEET_SEMILATTICE with type t = Req.t

(** {1 Manual construction}

    Advanced way to build a {!module-type:Preface_specs.MEET_SEMILATTICE},
    constructing and assembling a component-by-component of
    {!module-type:Preface_specs.MEET_SEMILATTICE}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Meet_semilattice.CORE)
    (Infix : Preface_specs.Meet_semilattice.INFIX with type t = Core.t) :
  Preface_specs.MEET_SEMILATTICE with type t = Infix.t

(** {2 Building Core} *)

module Core (Req : Preface_specs.Meet_semilattice.WITH_MEET) :
  Preface_specs.Meet_semilattice.CORE with type t = Req.t

(** {2 Deriving Infix} *)

module Infix (Core : Preface_specs.Meet_semilattice.CORE) :
  Preface_specs.Meet_semilattice.INFIX with type t = Core.t
