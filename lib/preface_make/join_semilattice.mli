(** Building a {!module:Preface_specs.Join_semilattice} *)

(** {1 Using the minimal definition}

    Build a {!module-type:Preface_specs.JOIN_SEMILATTICE} using
    {!module-type:Preface_specs.Join_semilattice.WITH_JOIN}. Standard method,
    using the minimal definition of a semigroup to derive its full API. *)

module Via_join (Req : Preface_specs.Join_semilattice.WITH_JOIN) :
  Preface_specs.JOIN_SEMILATTICE with type t = Req.t

(** {1 Manual construction}

    Advanced way to build a {!module-type:Preface_specs.JOIN_SEMILATTICE},
    constructing and assembling a component-by-component of
    {!module-type:Preface_specs.JOIN_SEMILATTICE}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Join_semilattice.CORE)
    (Infix : Preface_specs.Join_semilattice.INFIX with type t = Core.t) :
  Preface_specs.JOIN_SEMILATTICE with type t = Infix.t

(** {2 Building Core} *)

module Core (Req : Preface_specs.Join_semilattice.WITH_JOIN) :
  Preface_specs.Join_semilattice.CORE with type t = Req.t

(** {2 Deriving Infix} *)

module Infix (Core : Preface_specs.Join_semilattice.CORE) :
  Preface_specs.Join_semilattice.INFIX with type t = Core.t
