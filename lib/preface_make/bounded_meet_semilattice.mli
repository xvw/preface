(** Building a {!module:Preface_specs.Bounded_meet_semilattice} *)

(** {1 Using the minimal definition} *)

(** {2 Using meet and top}

    Build a {!module-type:Preface_specs.BOUNDED_MEET_SEMILATTICE} using
    {!module-type:Preface_specs.BOUNDED_MEET_SEMILATTICE.WITH_MEET_AND_TOP}. *)

module Via_meet_and_top
    (Req : Preface_specs.Bounded_meet_semilattice.WITH_MEET_AND_TOP) :
  Preface_specs.BOUNDED_MEET_SEMILATTICE with type t = Req.t

(** {1 Over Meet_semilattice}

    Produces a {!module-type:Preface_specs.BOUNDED_MEET_SEMILATTICE} from a
    {!module-type:Preface_specs.MEET_SEMILATTICE}. *)

module Over_meet_semilattice_and_via_top
    (Meet_req : Preface_specs.Meet_semilattice.CORE)
    (Req : Preface_specs.Bounded_meet_semilattice.WITH_TOP with type t = Meet_req.t) :
    Preface_specs.BOUNDED_MEET_SEMILATTICE with type t = Req.t

(** {1 Manual construction}

    Advanced way to build a
    {!module-type:Preface_specs.BOUNDED_MEET_SEMILATTICE}, constructing and
    assembling a component-by-component of
    {!module-type:Preface_specs.BOUNDED_MEET_SEMILATTICE}. (In order to provide
    your own implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Bounded_meet_semilattice.CORE)
    (Infix : Preface_specs.Bounded_meet_semilattice.INFIX with type t = Core.t) :
  Preface_specs.BOUNDED_MEET_SEMILATTICE with type t = Infix.t

(** {2 Building Core} *)

module Core_via_meet_and_top
    (Req : Preface_specs.Bounded_meet_semilattice.WITH_MEET_AND_TOP) :
  Preface_specs.Bounded_meet_semilattice.CORE with type t = Req.t

module Core_over_meet_semilattice_and_via_top
    (Meet_req : Preface_specs.Meet_semilattice.CORE)
    (Req : Preface_specs.Bounded_meet_semilattice.WITH_TOP
             with type t = Meet_req.t) :
  Preface_specs.Bounded_meet_semilattice.CORE with type t = Req.t

(** {2 Deriving Infix} *)

module Infix (Core : Preface_specs.Bounded_meet_semilattice.CORE) :
  Preface_specs.Bounded_meet_semilattice.INFIX with type t = Core.t
