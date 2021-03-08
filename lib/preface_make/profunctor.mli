(** {1 Documentation} *)

(** {2 Construction}

    Standard way to build a [Profunctor]. *)

(** Incarnation of a [Profunctor] with [dimap]. *)
module Via_dimap (Core : Preface_specs.Profunctor.CORE_WITH_DIMAP) :
  Preface_specs.PROFUNCTOR with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of a [Profunctor] with [contramap_fst] and [map_snd]. *)
module Via_contramap_fst_and_map_snd
    (Core : Preface_specs.Profunctor.CORE_WITH_CONTRAMAP_FST_AND_MAP_SND) :
  Preface_specs.PROFUNCTOR with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of a [Profunctor] using a [monad] using the Kleisli composition. *)
module From_monad (Monad : Preface_specs.Monad.CORE) :
  Preface_specs.PROFUNCTOR with type ('a, 'b) t = 'a -> 'b Monad.t
