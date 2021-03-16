(** {1 Documentation} *)

(** {2 Construction}

    Standard way to build a [Choice Profunctor]. *)

(** Incarnation of a [Choice Profunctor] with [dimap] and [left]. *)
module Via_dimap_and_left (Core : Preface_specs.Choice.CORE_WITH_DIMAP_AND_LEFT) :
  Preface_specs.Choice.CORE with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of a [Choice Profunctor] with [dimap] and [right]. *)
module Via_dimap_and_right
    (Core : Preface_specs.Choice.CORE_WITH_DIMAP_AND_RIGHT) :
  Preface_specs.Choice.CORE with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of a [Choice Profunctor] with [contramap_fst] and [map_snd] and
    [left]. *)
module Via_contramap_fst_and_map_snd_and_left
    (Core : Preface_specs.Choice.CORE_WITH_CONTRAMAP_FST_AND_MAP_SND_AND_LEFT) :
  Preface_specs.Choice.CORE with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of a [Choice Profunctor] with [contramap_fst] and [map_snd] and
    [right]. *)
module Via_contramap_fst_and_map_snd_and_right
    (Core : Preface_specs.Choice.CORE_WITH_CONTRAMAP_FST_AND_MAP_SND_AND_RIGHT) :
  Preface_specs.Choice.CORE with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of a [Choice Profunctor] over a [Profunctor] using [left]. *)
module Over_profunctor_via_left
    (P : Preface_specs.Profunctor.CORE)
    (L : Preface_specs.Choice.WITH_LEFT with type ('a, 'b) t = ('a, 'b) P.t) :
  Preface_specs.Choice.CORE with type ('a, 'b) t = ('a, 'b) L.t

(** Incarnation of a [Choice Profunctor] over a [Profunctor] using [right]. *)
module Over_profunctor_via_right
    (P : Preface_specs.Profunctor.CORE)
    (R : Preface_specs.Choice.WITH_RIGHT with type ('a, 'b) t = ('a, 'b) P.t) :
  Preface_specs.Choice.CORE with type ('a, 'b) t = ('a, 'b) R.t

(** Incarnation of a [Choice Profunctor] using a [monad] using the Kleisli
    composition. *)
module From_monad (Monad : Preface_specs.Monad.CORE) :
  Preface_specs.CHOICE with type ('a, 'b) t = 'a -> 'b Monad.t

(** {2 Choice Profunctors composition}

    Some tools for composition between Choice Profunctors. *)

(** Composition between two Choice Profunctors. *)
module Composition (F : Preface_specs.CHOICE) (G : Preface_specs.CHOICE) : sig
  type (_, _) t = Composed : (('a, 'b) F.t * ('b, 'c) G.t) -> ('a, 'c) t

  include Preface_specs.CHOICE with type ('a, 'b) t := ('a, 'b) t
end
