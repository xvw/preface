(** Building a {!module:Preface_specs.Choice} *)

(** {1 Using the minimal definition} *)

(** {2 Using dimap and left}

    Build a {!module-type:Preface_specs.CHOICE} using
    {!module-type:Preface_specs.Choice.WITH_DIMAP_AND_LEFT}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_dimap_and_left (Req : Preface_specs.Choice.WITH_DIMAP_AND_LEFT) :
  Preface_specs.CHOICE with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Using dimap and right}

    Build a {!module-type:Preface_specs.CHOICE} using
    {!module-type:Preface_specs.Choice.WITH_DIMAP_AND_RIGHT}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_dimap_and_right (Req : Preface_specs.Choice.WITH_DIMAP_AND_RIGHT) :
  Preface_specs.CHOICE with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Using contramap_fst, map_snd and left}

    Build a {!module-type:Preface_specs.CHOICE} using
    {!module-type:Preface_specs.Choice.WITH_CONTRAMAP_FST_AND_MAP_SND_AND_LEFT}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_contramap_fst_and_map_snd_and_left
    (Req : Preface_specs.Choice.WITH_CONTRAMAP_FST_AND_MAP_SND_AND_LEFT) :
  Preface_specs.CHOICE with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Using contramap_fst, map_snd and right}

    Build a {!module-type:Preface_specs.CHOICE} using
    {!module-type:Preface_specs.Choice.WITH_CONTRAMAP_FST_AND_MAP_SND_AND_RIGHT}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_contramap_fst_and_map_snd_and_right
    (Req : Preface_specs.Choice.WITH_CONTRAMAP_FST_AND_MAP_SND_AND_RIGHT) :
  Preface_specs.CHOICE with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Using left over a Profunctor}

    Build a {!module-type:Preface_specs.CHOICE} over a
    {!module-type:Preface_specs.PROFUNCTOR} using
    {!module-type:Preface_specs.Choice.WITH_LEFT}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_profunctor_via_left
    (P : Preface_specs.Profunctor.CORE)
    (L : Preface_specs.Choice.WITH_LEFT with type ('a, 'b) t = ('a, 'b) P.t) :
  Preface_specs.CHOICE with type ('a, 'b) t = ('a, 'b) L.t

(** {2 Using right over a Profunctor}

    Build a {!module-type:Preface_specs.CHOICE} over a
    {!module-type:Preface_specs.PROFUNCTOR} using
    {!module-type:Preface_specs.Choice.WITH_RIGHT}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_profunctor_via_right
    (P : Preface_specs.Profunctor.CORE)
    (R : Preface_specs.Choice.WITH_RIGHT with type ('a, 'b) t = ('a, 'b) P.t) :
  Preface_specs.CHOICE with type ('a, 'b) t = ('a, 'b) R.t

(** {1 Choice Algebra}

    Construction of {!module-type:Preface_specs.CHOICE} by combining them. *)

(** {2 Composition}

    Right-to-left composition of {!module-type:Preface_specs.CHOICE}.*)

module Composition (F : Preface_specs.CHOICE) (G : Preface_specs.CHOICE) : sig
  type (_, _) t = Composed : (('a, 'b) F.t * ('b, 'c) G.t) -> ('a, 'c) t

  include Preface_specs.CHOICE with type ('a, 'b) t := ('a, 'b) t
end

(** {1 From other abstraction} *)

(** {2 From a Monad}

    Produces a {!module-type:Preface_specs.CHOICE} from a
    {!module-type:Preface_specs.MONAD}. *)

module From_monad (Monad : Preface_specs.Monad.CORE) :
  Preface_specs.CHOICE with type ('a, 'b) t = 'a -> 'b Monad.t
