(** {1 Documentation} *)

(** {2 Construction}

    Standard way to build a [Closed Profunctor]. *)

(** Incarnation of a [Closed Profunctor] with [dimap] and [closed]. *)
module Via_dimap_and_closed
    (Core : Preface_specs.Closed.CORE_WITH_DIMAP_AND_CLOSED) :
  Preface_specs.CLOSED with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of a [Closed Profunctor] with [contramap_fst], [map_snd] and
    [closed]. *)
module Via_contramap_fst_and_map_snd_and_closed
    (Core : Preface_specs.Closed.CORE_WITH_CONTRAMAP_FST_AND_MAP_SND_AND_CLOSED) :
  Preface_specs.CLOSED with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of a [Closed Profunctor] over a [Profunctor] with [closed]. *)
module Over_profunctor_via_closed
    (P : Preface_specs.Profunctor.CORE)
    (C : Preface_specs.Closed.WITH_CLOSED with type ('a, 'b) t = ('a, 'b) P.t) :
  Preface_specs.CLOSED with type ('a, 'b) t = ('a, 'b) C.t

(** {2 Closed Profunctors composition}

    Some tools for composition between Closed Profunctors. *)

(** Composition between two Closed Profunctors. *)
module Composition (F : Preface_specs.CLOSED) (G : Preface_specs.CLOSED) : sig
  type (_, _) t = C : (('a, 'b) F.t * ('b, 'c) G.t) -> ('a, 'c) t

  include Preface_specs.CLOSED with type ('a, 'b) t := ('a, 'b) t
end

(** {2 Manual construction}

    Advanced way to build a [Closed Profunctor], constructing and assembling a
    component-by-component the closed profunctor. (In order to provide your own
    implementation for some features.) *)

(** Incarnation of a [Closed Profunctor] using each components of the
    [Closed Profunctor]. *)
module Via
    (Core : Preface_specs.Closed.CORE)
    (Operation : Preface_specs.Closed.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) :
  Preface_specs.CLOSED with type ('a, 'b) t = ('a, 'b) Operation.t

(** Incarnation of a [Closed.Core] with [dimap] and [closed]. *)
module Core_via_dimap_and_closed
    (Core : Preface_specs.Closed.CORE_WITH_DIMAP_AND_CLOSED) :
  Preface_specs.Closed.CORE with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of a [Closed.Core] with [contramap_fst], [map_snd] and [closed]. *)
module Core_via_contramap_fst_and_map_snd_and_closed
    (Core : Preface_specs.Closed.CORE_WITH_CONTRAMAP_FST_AND_MAP_SND_AND_CLOSED) :
  Preface_specs.Closed.CORE with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of a [Closed.Operation] with [Closed.Core]. *)
module Operation (Core : Preface_specs.Closed.CORE) :
  Preface_specs.Closed.OPERATION with type ('a, 'b) t = ('a, 'b) Core.t
