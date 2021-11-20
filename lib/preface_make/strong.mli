(** Building a {!module:Preface_specs.Strong} *)

(** {1 Using the minimal definition} *)

(** {2 Using dimap and fst}

    Build a {!module-type:Preface_specs.STRONG} using
    {!module-type:Preface_specs.Strong.WITH_DIMAP_AND_FST}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_dimap_and_fst (Req : Preface_specs.Strong.WITH_DIMAP_AND_FST) :
  Preface_specs.STRONG with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Using dimap and snd}

    Build a {!module-type:Preface_specs.STRONG} using
    {!module-type:Preface_specs.Strong.WITH_DIMAP_AND_SND}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_dimap_and_snd (Req : Preface_specs.Strong.WITH_DIMAP_AND_SND) :
  Preface_specs.STRONG with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Using contramap_fst, map_snd and fst}

    Build a {!module-type:Preface_specs.STRONG} using
    {!module-type:Preface_specs.Strong.WITH_CONTRAMAP_FST_AND_MAP_SND_AND_FST}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_contramap_fst_and_map_snd_and_fst
    (Req : Preface_specs.Strong.WITH_CONTRAMAP_FST_AND_MAP_SND_AND_FST) :
  Preface_specs.STRONG with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Using contramap_fst, map_snd and snd}

    Build a {!module-type:Preface_specs.STRONG} using
    {!module-type:Preface_specs.Strong.WITH_CONTRAMAP_FST_AND_MAP_SND_AND_SND}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_contramap_fst_and_map_snd_and_snd
    (Req : Preface_specs.Strong.WITH_CONTRAMAP_FST_AND_MAP_SND_AND_SND) :
  Preface_specs.STRONG with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Using fst over a Profunctor}

    Build a {!module-type:Preface_specs.STRONG} over a
    {!module-type:Preface_specs.PROFUNCTOR} using
    {!module-type:Preface_specs.Strong.WITH_FST}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_profunctor_via_fst
    (P : Preface_specs.PROFUNCTOR)
    (F : Preface_specs.Strong.WITH_FST with type ('a, 'b) t = ('a, 'b) P.t) :
  Preface_specs.STRONG with type ('a, 'b) t = ('a, 'b) F.t

(** {2 Using snd over a Profunctor}

    Build a {!module-type:Preface_specs.STRONG} over a
    {!module-type:Preface_specs.PROFUNCTOR} using
    {!module-type:Preface_specs.Strong.WITH_SND}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_profunctor_via_snd
    (P : Preface_specs.PROFUNCTOR)
    (S : Preface_specs.Strong.WITH_SND with type ('a, 'b) t = ('a, 'b) P.t) :
  Preface_specs.STRONG with type ('a, 'b) t = ('a, 'b) S.t

(** {1 Strong Algebra}

    Construction of {!module-type:Preface_specs.STRONG} by combining them. *)

(** {2 Composition}

    Right-to-left composition of {!module-type:Preface_specs.STRONG}.*)

module Composition (F : Preface_specs.STRONG) (G : Preface_specs.STRONG) : sig
  type (_, _) t = Composed : (('a, 'b) F.t * ('b, 'c) G.t) -> ('a, 'c) t

  include Preface_specs.STRONG with type ('a, 'b) t := ('a, 'b) t
end

(** {1 From other abstraction} *)

(** {2 From a Monad}

    Produces a {!module-type:Preface_specs.STRONG} from a
    {!module-type:Preface_specs.MONAD}. (Using Star)*)

module From_monad (Monad : Preface_specs.Monad.CORE) :
  Preface_specs.STRONG with type ('a, 'b) t = 'a -> 'b Monad.t

(** {2 From a Functor}

    Produces a {!module-type:Preface_specs.STRONG} from a
    {!module-type:Preface_specs.FUNCTOR}. (Using Star) *)

module From_functor (Functor : Preface_specs.Functor.CORE) :
  Preface_specs.STRONG with type ('a, 'b) t = 'a -> 'b Functor.t

(** {1 Manual construction}

    Advanced way to build a {!module-type:Preface_specs.STRONG}, constructing
    and assembling a component-by-component of
    {!module-type:Preface_specs.STRONG}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Strong.CORE)
    (Operation : Preface_specs.Strong.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) :
  Preface_specs.STRONG with type ('a, 'b) t = ('a, 'b) Operation.t

(** {2 Building Core} *)

module Core_over_profunctor_via_fst
    (P : Preface_specs.Profunctor.CORE)
    (F : Preface_specs.Strong.WITH_FST with type ('a, 'b) t = ('a, 'b) P.t) :
  Preface_specs.Strong.CORE with type ('a, 'b) t = ('a, 'b) F.t

module Core_over_profunctor_via_snd
    (P : Preface_specs.Profunctor.CORE)
    (S : Preface_specs.Strong.WITH_SND with type ('a, 'b) t = ('a, 'b) P.t) :
  Preface_specs.Strong.CORE with type ('a, 'b) t = ('a, 'b) S.t

module Core_via_dimap_and_fst (Req : Preface_specs.Strong.WITH_DIMAP_AND_FST) :
  Preface_specs.Strong.CORE with type ('a, 'b) t = ('a, 'b) Req.t

module Core_via_dimap_and_snd (Req : Preface_specs.Strong.WITH_DIMAP_AND_SND) :
  Preface_specs.Strong.CORE with type ('a, 'b) t = ('a, 'b) Req.t

module Core_via_contramap_fst_and_map_snd_and_fst
    (Req : Preface_specs.Strong.WITH_CONTRAMAP_FST_AND_MAP_SND_AND_FST) :
  Preface_specs.Strong.CORE with type ('a, 'b) t = ('a, 'b) Req.t

module Core_via_contramap_fst_and_map_snd_and_snd
    (Req : Preface_specs.Strong.WITH_CONTRAMAP_FST_AND_MAP_SND_AND_SND) :
  Preface_specs.Strong.CORE with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Strong.CORE) :
  Preface_specs.Strong.OPERATION with type ('a, 'b) t = ('a, 'b) Core.t
