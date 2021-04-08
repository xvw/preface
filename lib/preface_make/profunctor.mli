(** Building a {!module:Preface_specs.Profunctor} *)

(** {1 Using the minimal definition} *)

(** {2 Using dimap}

    Build a {!module-type:Preface_specs.PROFUNCTOR} using
    {!module-type:Preface_specs.Profunctor.WITH_DIMAP}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_dimap (Req : Preface_specs.Profunctor.WITH_DIMAP) :
  Preface_specs.PROFUNCTOR with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Using contramap_fst and map_snd}

    Build a {!module-type:Preface_specs.PROFUNCTOR} using
    {!module-type:Preface_specs.Profunctor.WITH_CONTRAMAP_FST_AND_MAP_SND}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_contramap_fst_and_map_snd
    (Req : Preface_specs.Profunctor.WITH_CONTRAMAP_FST_AND_MAP_SND) :
  Preface_specs.PROFUNCTOR with type ('a, 'b) t = ('a, 'b) Req.t

(** {1 Profunctor Algebra}

    Construction of {!module-type:Preface_specs.PROFUNCTOR} by combining them. *)

(** {2 Composition}

    Right-to-left composition of {!module-type:Preface_specs.PROFUNCTOR}.*)

module Composition (F : Preface_specs.PROFUNCTOR) (G : Preface_specs.PROFUNCTOR) : sig
  type (_, _) t = Composed : (('a, 'b) F.t * ('b, 'c) G.t) -> ('a, 'c) t

  include Preface_specs.PROFUNCTOR with type ('a, 'b) t := ('a, 'b) t
end

(** {1 From other abstraction} *)

(** {2 From a Monad}

    Produces a {!module-type:Preface_specs.PROFUNCTOR} from a
    {!module-type:Preface_specs.MONAD}. *)

module From_monad (Monad : Preface_specs.Monad.CORE) :
  Preface_specs.PROFUNCTOR with type ('a, 'b) t = 'a -> 'b Monad.t

(** {2 From a Strong profunctor}

    Produces a {!module-type:Preface_specs.PROFUNCTOR} from a
    {!module-type:Preface_specs.STRONG}. *)

module From_strong (Strong : Preface_specs.STRONG) :
  Preface_specs.PROFUNCTOR with type ('a, 'b) t = ('a, 'b) Strong.t

(** {2 From a Choice profunctor}

    Produces a {!module-type:Preface_specs.PROFUNCTOR} from a
    {!module-type:Preface_specs.CHOICE}. *)

module From_choice (Choice : Preface_specs.CHOICE) :
  Preface_specs.PROFUNCTOR with type ('a, 'b) t = ('a, 'b) Choice.t

(** {2 From a Closed profunctor}

    Produces a {!module-type:Preface_specs.PROFUNCTOR} from a
    {!module-type:Preface_specs.CLOSED}. *)

module From_closed (Closed : Preface_specs.CLOSED) :
  Preface_specs.PROFUNCTOR with type ('a, 'b) t = ('a, 'b) Closed.t

(** {1 Manual construction}

    Advanced way to build a {!module-type:Preface_specs.PROFUNCTOR},
    constructing and assembling a component-by-component of
    {!module-type:Preface_specs.PROFUNCTOR}. (In order to provide your own
    implementation for some features.) *)

(** {2 Building Core} *)

module Core_via_dimap (Req : Preface_specs.Profunctor.WITH_DIMAP) :
  Preface_specs.Profunctor.CORE with type ('a, 'b) t = ('a, 'b) Req.t

module Core_via_contramap_fst_and_map_snd
    (Req : Preface_specs.Profunctor.WITH_CONTRAMAP_FST_AND_MAP_SND) :
  Preface_specs.Profunctor.CORE with type ('a, 'b) t = ('a, 'b) Req.t
