(** Building a {!module:Preface_specs.Lettice} *)

(** {2 Using join_semilattice and meet_semilattice}

    Build a {!module-type:Preface_specs.LATTICE} using
    {!module-type:Preface_specs.LATTICE.WITH_JOIN_SEMILATTICE_AND_MEET_SEMILATTICE}. *)

module Via_join_and_meet (Req : Preface_specs.Lattice.WITH_JOIN_AND_MEET) :
  Preface_specs.LATTICE with type t = Req.t

(** {1 Over join_semilattice and over meet_semilattice semilattice}

    Produces a {!module-type:Preface_specs.LATTICE} from a
    {!module-type:Preface_specs.JOIN_SEMILATTICE}. and a
    {!module-type:Preface_specs.MEET_SEMILATTICE}. *)

module Over_join_semilattice_and_meet_semilattice
    (Join_req : Preface_specs.Join_semilattice.CORE)
    (Meet_req : Preface_specs.Meet_semilattice.CORE with type t = Join_req.t) :
  Preface_specs.LATTICE with type t = Meet_req.t

(** {1 Manual construction}

    Advanced way to build a {!module-type:Preface_specs.LATTICE}, constructing
    and assembling a component-by-component of
    {!module-type:Preface_specs.LATTICE}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Lattice.CORE)
    (Infix : Preface_specs.Lattice.INFIX with type t = Core.t) :
  Preface_specs.LATTICE with type t = Infix.t

(** {2 Building Core} *)

module Core_via_join_and_meet (Req : Preface_specs.Lattice.WITH_JOIN_AND_MEET) :
  Preface_specs.Lattice.CORE with type t = Req.t

(** {2 Deriving Infix} *)

module Infix (Core : Preface_specs.Lattice.CORE) :
  Preface_specs.Lattice.INFIX with type t = Core.t
