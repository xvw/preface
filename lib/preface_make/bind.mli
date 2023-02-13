(** Building a {!module:Preface_specs.Bind} *)

(** {1 Using the minimal definition} *)

(** {2 Using map and bind}

    Build a {!module-type:Preface_specs.Bind} using
    {!module-type:Preface_specs.Bind.WITH_RETURN_AND_BIND}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_map_and_bind (Req : Preface_specs.Bind.WITH_MAP_AND_BIND) :
  Preface_specs.BIND with type 'a t = 'a Req.t

(** {2 Using bind over functor}

    Build a {!module-type:Preface_specs.BIND} using
    {!module-type:Preface_specs.Functor.WITH_MAP} and
    {!module-type:Preface_specs.Bind.WITH_BIND}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_functor_via_bind
    (Functor : Preface_specs.Functor.WITH_MAP)
    (Req : Preface_specs.Bind.WITH_BIND with type 'a t = 'a Functor.t) :
  Preface_specs.BIND with type 'a t = 'a Req.t

(** {2 Using return, map and join}

    Build a {!module-type:Preface_specs.BIND} using
    {!module-type:Preface_specs.Bind.WITH_RETURN_MAP_AND_JOIN}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_map_and_join (Req : Preface_specs.Bind.WITH_MAP_AND_JOIN) :
  Preface_specs.BIND with type 'a t = 'a Req.t

(** {2 Using return and the kleisli composition}

    Build a {!module-type:Preface_specs.BIND} using
    {!module-type:Preface_specs.Bind.WITH_RETURN_AND_KLEISLI_COMPOSITION}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_map_and_kleisli_composition
    (Req : Preface_specs.Bind.WITH_MAP_AND_KLEISLI_COMPOSITION) :
  Preface_specs.BIND with type 'a t = 'a Req.t

(** {2 Using kleisli composition over functor}

    Build a {!module-type:Preface_specs.BIND} using
    {!module-type:Preface_specs.Functor.WITH_MAP} and
    {!module-type:Preface_specs.Bind.WITH_KLEISLI_COMPOSITION}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_functor_via_kleisli_composition
    (Functor : Preface_specs.Functor.WITH_MAP)
    (Req : Preface_specs.Bind.WITH_KLEISLI_COMPOSITION
             with type 'a t = 'a Functor.t) :
  Preface_specs.BIND with type 'a t = 'a Req.t

(** {1 Bind Algebra}

    Construction of {!module-type:Preface_specs.BIND} by combining them. *)

(** {2 Product}

    Construct the product of two {!module-type:Preface_specs.BIND}. *)

module Product (F : Preface_specs.BIND) (G : Preface_specs.BIND) :
  Preface_specs.BIND with type 'a t = 'a F.t * 'a G.t

(** {1 From other abstraction} *)

(** {2 From a Monad}

    Produces a {!module-type:Preface_specs.BIND} from an
    {!module-type:Preface_specs.MONAD}. *)

module From_monad (Monad : Preface_specs.MONAD) :
  Preface_specs.BIND with type 'a t = 'a Monad.t

(** {2 From a Monad plus}

    Produces a {!module-type:Preface_specs.BIND} from an
    {!module-type:Preface_specs.MONAD_PLUS}. *)

module From_monad_plus (Monad_plus : Preface_specs.MONAD_PLUS) :
  Preface_specs.BIND with type 'a t = 'a Monad_plus.t

(** {2 From an Arrow Apply}

    Produces a {!module-type:Preface_specs.BIND} from an
    {!module-type:Preface_specs.ARROW_APPLY}. *)

module From_arrow_apply (A : Preface_specs.ARROW_APPLY) :
  Preface_specs.BIND with type 'a t = (unit, 'a) A.t

(** {1 To other abstraction} *)

(** {2 To an Indexed Bind} *)

module Index (F : Preface_specs.BIND) :
  Preface_specs.INDEXED_BIND with type ('a, 'index) t = 'a F.t

(** {1 Manual construction}

    Advanced way to build a {!module-type:Preface_specs.BIND}, constructing and
    assembling a component-by-component of {!module-type:Preface_specs.BIND}.
    (In order to provide your own implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Bind.CORE)
    (Operation : Preface_specs.Bind.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Bind.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Bind.SYNTAX with type 'a t = 'a Core.t) :
  Preface_specs.BIND with type 'a t = 'a Core.t

(** {2 Building Core} *)

module Core_via_map_and_bind (Req : Preface_specs.Bind.WITH_MAP_AND_BIND) :
  Preface_specs.Bind.CORE with type 'a t = 'a Req.t

module Core_via_map_and_join (Req : Preface_specs.Bind.WITH_MAP_AND_JOIN) :
  Preface_specs.Bind.CORE with type 'a t = 'a Req.t

module Core_via_map_and_kleisli_composition
    (Req : Preface_specs.Bind.WITH_MAP_AND_KLEISLI_COMPOSITION) :
  Preface_specs.Bind.CORE with type 'a t = 'a Req.t

module Core_over_functor_via_bind
    (Functor : Preface_specs.Functor.WITH_MAP)
    (Req : Preface_specs.Bind.WITH_BIND with type 'a t = 'a Functor.t) :
  Preface_specs.Bind.CORE with type 'a t = 'a Req.t

module Core_over_functor_via_kleisli_composition
    (Functor : Preface_specs.Functor.WITH_MAP)
    (Req : Preface_specs.Bind.WITH_KLEISLI_COMPOSITION
             with type 'a t = 'a Functor.t) :
  Preface_specs.Bind.CORE with type 'a t = 'a Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Bind.CORE) :
  Preface_specs.Bind.OPERATION with type 'a t = 'a Core.t

(** {2 Deriving Syntax} *)

module Syntax (Core : Preface_specs.Bind.CORE) :
  Preface_specs.Bind.SYNTAX with type 'a t = 'a Core.t

(** {2 Deriving Infix} *)

module Infix
    (Core : Preface_specs.Bind.CORE)
    (Operation : Preface_specs.Bind.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Bind.INFIX with type 'a t = 'a Core.t
