(** Building a {!module:Preface_specs.Bounded_lettice} *)

(** {2 Using bounded join_lattice semilattice and bounded meet_lattice
    semilattice}

    Build a {!module-type:Preface_specs.BOUNDED_LATTICE} using
    {!module-type:Preface_specs.BOUNDED_LATTICE.WITH_BOUNDED_JOIN_AND_BOUNDED_MEET}. *)

module Via_bounded_join_lattice_and_bounded_meet_lattice
    (Req : Preface_specs.Bounded_lattice
           .WITH_BOUNDED_JOIN_LATTICE_AND_BOUNDED_MEET_LATTICE) :
  Preface_specs.BOUNDED_LATTICE with type t = Req.t

(** {1 Over Bounded join_lattice semilattice and over bounded meet_lattice
    semilattice}

    Produces a {!module-type:Preface_specs.BOUNDED_LATTICE} from a
    {!module-type:Preface_specs.BOUNDED_JOIN_SEMILATTICE}. and a
    {!module-type:Preface_specs.BOUNDED_MEET_SEMILATTICE}. *)

module Over_bounded_join_lattice_and_bounded_meet_lattice
    (Join_req : Preface_specs.Bounded_join_semilattice.CORE)
    (Meet_req : Preface_specs.Bounded_meet_semilattice.CORE
                  with type t = Join_req.t) :
  Preface_specs.BOUNDED_LATTICE with type t = Meet_req.t

(** {1 Manual construction}

    Advanced way to build a {!module-type:Preface_specs.BOUNDED_LATTICE},
    constructing and assembling a component-by-component of
    {!module-type:Preface_specs.BOUNDED_LATTICE}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Bounded_lattice.CORE)
    (Infix : Preface_specs.Bounded_lattice.INFIX with type t = Core.t) :
  Preface_specs.BOUNDED_LATTICE with type t = Infix.t

(** {2 Building Core} *)

module Core_via_bounded_join_lattice_and_bounded_meet_lattice
    (Req : Preface_specs.Bounded_lattice
           .WITH_BOUNDED_JOIN_LATTICE_AND_BOUNDED_MEET_LATTICE) :
  Preface_specs.Bounded_lattice.CORE with type t = Req.t

module Core_over_bounded_join_lattice_and_bounded_meet_lattice
    (Join_req : Preface_specs.Bounded_join_semilattice.CORE)
    (Meet_req : Preface_specs.Bounded_meet_semilattice.CORE
                  with type t = Join_req.t) :
  Preface_specs.Bounded_lattice.CORE with type t = Meet_req.t

(** {2 Deriving Infix} *)

module Infix (Core : Preface_specs.Bounded_lattice.CORE) :
  Preface_specs.Bounded_lattice.INFIX with type t = Core.t
