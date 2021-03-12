(** {1 Documentation} *)

(** {2 Construction}

    Standard way to build a [Closed Profunctor]. *)

(** Incarnation of a [Closed Profunctor] with [dimap] and [closed]. *)
module Via_dimap_and_closed
    (Core : Preface_specs.Closed.CORE_WITH_DIMAP_AND_CLOSED) :
  Preface_specs.Closed.CORE with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of a [Closed Profunctor] with [contramap_fst], [map_snd] and
    [closed]. *)
module Via_contramap_fst_and_map_snd_and_closed
    (Core : Preface_specs.Closed.CORE_WITH_CONTRAMAP_FST_AND_MAP_SND_AND_CLOSED) :
  Preface_specs.Closed.CORE with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of a [Closed Profunctor] over a [Profunctor] with [closed]. *)
module Over_profunctor_via_closed
    (P : Preface_specs.Profunctor.CORE)
    (C : Preface_specs.Closed.WITH_CLOSED with type ('a, 'b) t = ('a, 'b) P.t) :
  Preface_specs.Closed.CORE with type ('a, 'b) t = ('a, 'b) C.t
