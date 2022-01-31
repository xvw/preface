(** Building a {!module:Preface_specs.Monad} *)

(** {1 Using the minimal definition} *)

(** {2 Using return and bind}

    Build a {!module-type:Preface_specs.MONAD} using
    {!module-type:Preface_specs.Monad.WITH_RETURN_AND_BIND}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_return_and_bind (Req : Preface_specs.Monad.WITH_RETURN_AND_BIND) :
  Preface_specs.MONAD with type 'a t = 'a Req.t

(** {2 Using return, map and join}

    Build a {!module-type:Preface_specs.MONAD} using
    {!module-type:Preface_specs.Monad.WITH_RETURN_MAP_AND_JOIN}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_return_map_and_join
    (Req : Preface_specs.Monad.WITH_RETURN_MAP_AND_JOIN) :
  Preface_specs.MONAD with type 'a t = 'a Req.t

(** {2 Using return and the kleisli composition}

    Build a {!module-type:Preface_specs.MONAD} using
    {!module-type:Preface_specs.Monad.WITH_RETURN_AND_KLEISLI_COMPOSITION}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_return_and_kleisli_composition
    (Req : Preface_specs.Monad.WITH_RETURN_AND_KLEISLI_COMPOSITION) :
  Preface_specs.MONAD with type 'a t = 'a Req.t

(** {1 Monad Algebra}

    Construction of {!module-type:Preface_specs.MONAD} by combining them. *)

(** {2 Product}

    Construct the product of two {!module-type:Preface_specs.MONAD}. *)

module Product (F : Preface_specs.MONAD) (G : Preface_specs.MONAD) :
  Preface_specs.MONAD with type 'a t = 'a F.t * 'a G.t

(** {1 From other abstraction} *)

(** {2 From a Monad Plus}

    Produces a {!module-type:Preface_specs.MONAD} from a
    {!module-type:Preface_specs.MONAD_PLUS}. *)

module From_monad_plus (Monad_plus : Preface_specs.MONAD_PLUS) :
  Preface_specs.MONAD with type 'a t = 'a Monad_plus.t

(** {2 From an Arrow Apply}

    Produces a {!module-type:Preface_specs.MONAD} from an
    {!module-type:Preface_specs.ARROW_APPLY}. *)

module From_arrow_apply (A : Preface_specs.ARROW_APPLY) :
  Preface_specs.MONAD with type 'a t = (unit, 'a) A.t

(** {1 Manual construction}

    Advanced way to build a {!module-type:Preface_specs.MONAD}, constructing and
    assembling a component-by-component of {!module-type:Preface_specs.MONAD}.
    (In order to provide your own implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Monad.CORE)
    (Operation : Preface_specs.Monad.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Monad.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Monad.SYNTAX with type 'a t = 'a Core.t) :
  Preface_specs.MONAD with type 'a t = 'a Core.t

(** {2 Building Core} *)

module Core_via_return_and_bind (Req : Preface_specs.Monad.WITH_RETURN_AND_BIND) :
  Preface_specs.Monad.CORE with type 'a t = 'a Req.t

module Core_via_return_map_and_join
    (Req : Preface_specs.Monad.WITH_RETURN_MAP_AND_JOIN) :
  Preface_specs.Monad.CORE with type 'a t = 'a Req.t

module Core_via_return_and_kleisli_composition
    (Req : Preface_specs.Monad.WITH_RETURN_AND_KLEISLI_COMPOSITION) :
  Preface_specs.Monad.CORE with type 'a t = 'a Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Monad.CORE) :
  Preface_specs.Monad.OPERATION with type 'a t = 'a Core.t

(** {2 Deriving Syntax} *)

module Syntax (Core : Preface_specs.Monad.CORE) :
  Preface_specs.Monad.SYNTAX with type 'a t = 'a Core.t

(** {2 Deriving Infix} *)

module Infix
    (Core : Preface_specs.Monad.CORE)
    (Operation : Preface_specs.Monad.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Monad.INFIX with type 'a t = 'a Core.t
