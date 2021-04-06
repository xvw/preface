(** Building a {!module:Preface_specs.Closed} *)

(** {1 Using the minimal definition} *)

(** {2 Using dimap and closed}

    Build a {!module-type:Preface_specs.CLOSED} using
    {!module-type:Preface_specs.Closed.WITH_DIMAP_AND_CLOSED}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_dimap_and_closed (Req : Preface_specs.Closed.WITH_DIMAP_AND_CLOSED) :
  Preface_specs.CLOSED with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Using contramap_fst, map_snd and closed}

    Build a {!module-type:Preface_specs.CLOSED} using
    {!module-type:Preface_specs.Closed.WITH_CONTRAMAP_FST_AND_MAP_SND_AND_CLOSED}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_contramap_fst_and_map_snd_and_closed
    (Req : Preface_specs.Closed.WITH_CONTRAMAP_FST_AND_MAP_SND_AND_CLOSED) :
  Preface_specs.CLOSED with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Using closed over a Profunctor}

    Build a {!module-type:Preface_specs.CLOSED} over a
    {!module-type:Preface_specs.PROFUNCTOR} using
    {!module-type:Preface_specs.Closed.WITH_CLOSED}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_profunctor_via_closed
    (P : Preface_specs.Profunctor.CORE)
    (C : Preface_specs.Closed.WITH_CLOSED with type ('a, 'b) t = ('a, 'b) P.t) :
  Preface_specs.CLOSED with type ('a, 'b) t = ('a, 'b) C.t

(** {1 Closed Algebra}

    Construction of {!module-type:Preface_specs.CLOSED} by combining them. *)

(** {2 Composition}

    Right-to-left composition of {!module-type:Preface_specs.CLOSED}.*)

module Composition (F : Preface_specs.CLOSED) (G : Preface_specs.CLOSED) : sig
  type (_, _) t = Composed : (('a, 'b) F.t * ('b, 'c) G.t) -> ('a, 'c) t

  include Preface_specs.CLOSED with type ('a, 'b) t := ('a, 'b) t
end

(** {1 Manual construction}

    Advanced way to build a {!module-type:Preface_specs.CLOSED}, constructing
    and assembling a component-by-component of
    {!module-type:Preface_specs.CLOSED}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Closed.CORE)
    (Operation : Preface_specs.Closed.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) :
  Preface_specs.CLOSED with type ('a, 'b) t = ('a, 'b) Operation.t

(** {2 Building Core} *)

module Core_via_dimap_and_closed
    (Req : Preface_specs.Closed.WITH_DIMAP_AND_CLOSED) :
  Preface_specs.Closed.CORE with type ('a, 'b) t = ('a, 'b) Req.t

module Core_via_contramap_fst_and_map_snd_and_closed
    (Req : Preface_specs.Closed.WITH_CONTRAMAP_FST_AND_MAP_SND_AND_CLOSED) :
  Preface_specs.Closed.CORE with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Closed.CORE) :
  Preface_specs.Closed.OPERATION with type ('a, 'b) t = ('a, 'b) Core.t
