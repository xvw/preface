(** Building a {!module:Preface_specs.Bounded_join_semilattice} *)

(** {1 Using the minimal definition}

    Build a {!module-type:Preface_specs.BOUNDED_JOIN_SEMILATTICE} using
    {!module-type:Preface_specs.Bounded_join_semilattice.WITH_JOIN}. Standard
    method, using the minimal definition of a semigroup to derive its full API. *)

(** {2 Using join and bottom}

    Build a {!module-type:Preface_specs.BOUNDED_JOIN_SEMILATTICE} using
    {!module-type:Preface_specs.BOUNDED_JOIN_SEMILATTICE.WITH_JOIN_AND_BOTTOM}. *)

module Via_join_and_bottom
    (Req : Preface_specs.Bounded_join_semilattice.WITH_JOIN_AND_BOTTOM) :
  Preface_specs.BOUNDED_JOIN_SEMILATTICE with type t = Req.t

(** {1 Over Join_semilattice}

    Produces a {!module-type:Preface_specs.BOUNDED_JOIN_SEMILATTICE} from a
    {!module-type:Preface_specs.JOIN_SEMILATTICE}. *)

module Over_join_semilattice_and_via_bottom
    (Join_req : Preface_specs.Join_semilattice.CORE)
    (Req : Preface_specs.Bounded_join_semilattice.WITH_BOTTOM
             with type t = Join_req.t) :
  Preface_specs.BOUNDED_JOIN_SEMILATTICE with type t = Req.t

(** {1 Manual construction}

    Advanced way to build a
    {!module-type:Preface_specs.BOUNDED_JOIN_SEMILATTICE}, constructing and
    assembling a component-by-component of
    {!module-type:Preface_specs.BOUNDED_JOIN_SEMILATTICE}. (In order to provide
    your own implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Bounded_join_semilattice.CORE)
    (Infix : Preface_specs.Bounded_join_semilattice.INFIX with type t = Core.t) :
  Preface_specs.BOUNDED_JOIN_SEMILATTICE with type t = Infix.t

(** {2 Building Core} *)

module Core_via_join_and_bottom
    (Req : Preface_specs.Bounded_join_semilattice.WITH_JOIN_AND_BOTTOM) :
  Preface_specs.Bounded_join_semilattice.CORE with type t = Req.t

module Core_over_join_semilattice_and_via_bottom
    (Join_req : Preface_specs.Join_semilattice.CORE)
    (Req : Preface_specs.Bounded_join_semilattice.WITH_BOTTOM
             with type t = Join_req.t) :
  Preface_specs.Bounded_join_semilattice.CORE with type t = Req.t

(** {2 Deriving Infix} *)

module Infix (Core : Preface_specs.Bounded_join_semilattice.CORE) :
  Preface_specs.Bounded_join_semilattice.INFIX with type t = Core.t
