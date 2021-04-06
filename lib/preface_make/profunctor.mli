(** {1 Documentation} *)

(** {2 Construction}

    Standard way to build a [Profunctor]. *)

(** Incarnation of a [Profunctor] with [dimap]. *)
module Via_dimap (Req : Preface_specs.Profunctor.WITH_DIMAP) :
  Preface_specs.PROFUNCTOR with type ('a, 'b) t = ('a, 'b) Req.t

(** Incarnation of a [Profunctor] with [contramap_fst] and [map_snd]. *)
module Via_contramap_fst_and_map_snd
    (Req : Preface_specs.Profunctor.WITH_CONTRAMAP_FST_AND_MAP_SND) :
  Preface_specs.PROFUNCTOR with type ('a, 'b) t = ('a, 'b) Req.t

(** Incarnation of a [Profunctor] using a [monad] using the Kleisli composition. *)
module From_monad (Monad : Preface_specs.Monad.CORE) :
  Preface_specs.PROFUNCTOR with type ('a, 'b) t = 'a -> 'b Monad.t

(** Convert a [Strong Profunctor] into a [Profunctor]. *)
module From_strong (Strong : Preface_specs.STRONG) :
  Preface_specs.PROFUNCTOR with type ('a, 'b) t = ('a, 'b) Strong.t

(** Convert a [Choice Profunctor] into a [Profunctor]. *)
module From_choice (Choice : Preface_specs.CHOICE) :
  Preface_specs.PROFUNCTOR with type ('a, 'b) t = ('a, 'b) Choice.t

(** Convert a [Closed Profunctor] into a [Profunctor]. *)
module From_closed (Closed : Preface_specs.CLOSED) :
  Preface_specs.PROFUNCTOR with type ('a, 'b) t = ('a, 'b) Closed.t

(** {2 Profunctors composition}

    Some tools for composition between Profunctors. *)

(** Composition between two Profunctors. *)
module Composition (F : Preface_specs.PROFUNCTOR) (G : Preface_specs.PROFUNCTOR) : sig
  type (_, _) t = Composed : (('a, 'b) F.t * ('b, 'c) G.t) -> ('a, 'c) t

  include Preface_specs.PROFUNCTOR with type ('a, 'b) t := ('a, 'b) t
end

(** {2 Manual construction}

    Advanced way to build a [Profunctor], constructing and assembling a
    component-by-component a Profunctor. (In order to provide your own
    implementation for some features.) *)

(** Incarnation of a [Profunctor.Core] with [dimap]. *)
module Core_via_dimap (Req : Preface_specs.Profunctor.WITH_DIMAP) :
  Preface_specs.Profunctor.CORE with type ('a, 'b) t = ('a, 'b) Req.t

(** Incarnation of a [Profunctor.Core] with [contramap_fst] and [map_snd]. *)
module Core_via_contramap_fst_and_map_snd
    (Req : Preface_specs.Profunctor.WITH_CONTRAMAP_FST_AND_MAP_SND) :
  Preface_specs.Profunctor.CORE with type ('a, 'b) t = ('a, 'b) Req.t
