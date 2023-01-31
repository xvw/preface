(** Building a {!module:Preface_specs.Monad_plus} *)

(** {1 Using the minimal definition} *)

(** {2 Using return, bind, neutral and combine}

    Build a {!module-type:Preface_specs.MONAD_PLUS} using
    {!module-type:Preface_specs.Monad_plus.WITH_BIND}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_bind (Req : Preface_specs.Monad_plus.WITH_BIND) :
  Preface_specs.MONAD_PLUS with type 'a t = 'a Req.t

(** {2 Using return, map, join, neutral and combine}

    Build a {!module-type:Preface_specs.MONAD_PLUS} using
    {!module-type:Preface_specs.Monad_plus.WITH_MAP_AND_JOIN}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_map_and_join (Req : Preface_specs.Monad_plus.WITH_MAP_AND_JOIN) :
  Preface_specs.MONAD_PLUS with type 'a t = 'a Req.t

(** {2 Using return, neutral, combine and the kleisli composition}

    Build a {!module-type:Preface_specs.MONAD_PLUS} using
    {!module-type:Preface_specs.Monad_plus.WITH_KLEISLI_COMPOSITION}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_kleisli_composition
    (Req : Preface_specs.Monad_plus.WITH_KLEISLI_COMPOSITION) :
  Preface_specs.MONAD_PLUS with type 'a t = 'a Req.t

(** {2 Over a Monad and an Alternative}

    Build a {!module-type:Preface_specs.MONAD_PLUS} over a
    {!module-type:Preface_specs.MONAD} and an [neutral] and [combine] from an
    {!module-type:Preface_specs.ALTERNATIVE}. *)

module Over_monad_and_alternative
    (Monad : Preface_specs.MONAD)
    (Alternative : Preface_specs.ALTERNATIVE with type 'a t = 'a Monad.t) :
  Preface_specs.MONAD_PLUS with type 'a t = 'a Alternative.t

(** {2 Over a Monad using neutral and combine}

    Build a {!module-type:Preface_specs.MONAD_PLUS} over a
    {!module-type:Preface_specs.MONAD} and using
    {!module-type:Preface_specs.Monad_plus.WITH_NEUTRAL_AND_COMBINE}. *)

module Over_monad
    (Monad : Preface_specs.MONAD)
    (Req : Preface_specs.Monad_plus.WITH_NEUTRAL_AND_COMBINE
             with type 'a t = 'a Monad.t) :
  Preface_specs.MONAD_PLUS with type 'a t = 'a Req.t

(** {1 Monad plus Algebra}

    Construction of {!module-type:Preface_specs.MONAD_PLUS} by combining them. *)

(** {2 Product}

    Construct the product of two {!module-type:Preface_specs.MONAD_PLUS}. *)

module Product (F : Preface_specs.MONAD_PLUS) (G : Preface_specs.MONAD_PLUS) :
  Preface_specs.MONAD_PLUS with type 'a t = 'a F.t * 'a G.t

(** {1 From other abstraction} *)

(** {2 From an Arrow Plus and Arrow Apply}

    Produces an {!module-type:Preface_specs.MONAD_PLUS} from an [Arrow] which
    has to be a {!module-type:Preface_specs.ARROW_PLUS} and a
    {!module-type:Preface_specs.ARROW_APPLY}. *)

module From_arrow_apply_and_arrow_plus
    (A : Preface_specs.ARROW_APPLY)
    (P : Preface_specs.ARROW_PLUS with type ('a, 'b) t = ('a, 'b) A.t) :
  Preface_specs.MONAD_PLUS with type 'a t = (unit, 'a) P.t

(** {1 To other abstraction} *)

(** {2 To an Indexed Monad} *)

module Index (F : Preface_specs.MONAD_PLUS) :
  Preface_specs.INDEXED_MONAD_PLUS with type ('a, 'index) t = 'a F.t

(** {1 Manual construction}

    Advanced way to build a {!module-type:Preface_specs.MONAD_PLUS},
    constructing and assembling a component-by-component of
    {!module-type:Preface_specs.MONAD_PLUS}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Monad_plus.CORE)
    (Operation : Preface_specs.Monad_plus.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Monad_plus.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Monad_plus.SYNTAX with type 'a t = 'a Core.t) :
  Preface_specs.MONAD_PLUS with type 'a t = 'a Core.t

(** {2 Building Core} *)

module Core_via_bind (Req : Preface_specs.Monad_plus.WITH_BIND) :
  Preface_specs.Monad_plus.CORE with type 'a t = 'a Req.t

module Core_via_map_and_join (Req : Preface_specs.Monad_plus.WITH_MAP_AND_JOIN) :
  Preface_specs.Monad_plus.CORE with type 'a t = 'a Req.t

module Core_via_kleisli_composition
    (Req : Preface_specs.Monad_plus.WITH_KLEISLI_COMPOSITION) :
  Preface_specs.Monad_plus.CORE with type 'a t = 'a Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Monad_plus.CORE) :
  Preface_specs.Monad_plus.OPERATION with type 'a t = 'a Core.t

(** {2 Deriving Syntax} *)

module Syntax (Core : Preface_specs.Monad_plus.CORE) :
  Preface_specs.Monad_plus.SYNTAX with type 'a t = 'a Core.t

(** {2 Deriving Infix} *)

module Infix
    (Core : Preface_specs.Monad_plus.CORE)
    (Operation : Preface_specs.Monad_plus.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Monad_plus.INFIX with type 'a t = 'a Core.t
