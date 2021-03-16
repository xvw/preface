(** {1 Documentation} *)

(** {2 Construction}

    Standard way to build a [Strong Profunctor]. *)

(** Incarnation of a [Strong Profunctor] with [dimap] and [fst]. *)
module Via_dimap_and_fst (Core : Preface_specs.Strong.CORE_WITH_DIMAP_AND_FST) :
  Preface_specs.STRONG with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of a [Strong Profunctor] with [dimap] and [snd]. *)
module Via_dimap_and_snd (Core : Preface_specs.Strong.CORE_WITH_DIMAP_AND_SND) :
  Preface_specs.STRONG with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of a [Strong Profunctor] with [contramap_fst] and [map_snd] and
    [fst]. *)
module Via_contramap_fst_and_map_snd_and_fst
    (Core : Preface_specs.Strong.CORE_WITH_CONTRAMAP_FST_AND_MAP_SND_AND_FST) :
  Preface_specs.STRONG with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of a [Strong Profunctor] with [contramap_fst] and [map_snd] and
    [snd]. *)
module Via_contramap_fst_and_map_snd_and_snd
    (Core : Preface_specs.Strong.CORE_WITH_CONTRAMAP_FST_AND_MAP_SND_AND_SND) :
  Preface_specs.STRONG with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of a [Strong Profunctor] over a [Profunctor] using [fst]. *)
module Over_profunctor_via_fst
    (P : Preface_specs.PROFUNCTOR)
    (F : Preface_specs.Strong.WITH_FST with type ('a, 'b) t = ('a, 'b) P.t) :
  Preface_specs.STRONG with type ('a, 'b) t = ('a, 'b) F.t

(** Incarnation of a [Strong Profunctor] over a [Profunctor] using [snd]. *)
module Over_profunctor_via_snd
    (P : Preface_specs.PROFUNCTOR)
    (S : Preface_specs.Strong.WITH_SND with type ('a, 'b) t = ('a, 'b) P.t) :
  Preface_specs.STRONG with type ('a, 'b) t = ('a, 'b) S.t

(** Incarnation of a [Strong Profunctor] using a [monad] using the Kleisli
    composition. *)
module From_monad (Monad : Preface_specs.Monad.CORE) :
  Preface_specs.STRONG with type ('a, 'b) t = 'a -> 'b Monad.t

(** {2 Manual construction}

    Advanced way to build a [Strong Profunctor], constructing and assembling a
    component-by-component the strong profunctor. (In order to provide your own
    implementation for some features.) *)

(** Incarnation of a [Strong Profunctor] using each components of the
    [Strong Profunctor]. *)
module Via
    (Core : Preface_specs.Strong.CORE)
    (Operation : Preface_specs.Strong.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) :
  Preface_specs.STRONG with type ('a, 'b) t = ('a, 'b) Operation.t

(** Incarnation of [Strong.Core] over a [Profunctor] using [fst]. *)
module Core_over_profunctor_via_fst
    (P : Preface_specs.Profunctor.CORE)
    (F : Preface_specs.Strong.WITH_FST with type ('a, 'b) t = ('a, 'b) P.t) :
  Preface_specs.Strong.CORE with type ('a, 'b) t = ('a, 'b) F.t

(** Incarnation of [Strong.Core] over a [Profunctor] using [snd]. *)
module Core_over_profunctor_via_snd
    (P : Preface_specs.Profunctor.CORE)
    (S : Preface_specs.Strong.WITH_SND with type ('a, 'b) t = ('a, 'b) P.t) :
  Preface_specs.Strong.CORE with type ('a, 'b) t = ('a, 'b) S.t

(** Incarnation of [Strong.Core] over a [Profunctor] using [dimap] and [fst]. *)
module Core_via_dimap_and_fst
    (Core : Preface_specs.Strong.CORE_WITH_DIMAP_AND_FST) :
  Preface_specs.Strong.CORE with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of [Strong.Core] over a [Profunctor] using [dimap] and [snd]. *)
module Core_via_dimap_and_snd
    (Core : Preface_specs.Strong.CORE_WITH_DIMAP_AND_SND) :
  Preface_specs.Strong.CORE with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of a [Strong.Core] with [contramap_fst] and [map_snd] and [fst]. *)
module Core_via_contramap_fst_and_map_snd_and_fst
    (Core : Preface_specs.Strong.CORE_WITH_CONTRAMAP_FST_AND_MAP_SND_AND_FST) :
  Preface_specs.Strong.CORE with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of a [Strong.Core] with [contramap_fst] and [map_snd] and [snd]. *)
module Core_via_contramap_fst_and_map_snd_and_snd
    (Core : Preface_specs.Strong.CORE_WITH_CONTRAMAP_FST_AND_MAP_SND_AND_SND) :
  Preface_specs.Strong.CORE with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of a [Strong.Operation] using [Core]. *)
module Operation (Core : Preface_specs.Strong.CORE) :
  Preface_specs.Strong.OPERATION with type ('a, 'b) t = ('a, 'b) Core.t
