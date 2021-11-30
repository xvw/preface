(** Building a {!module:Preface_specs.Semigroupoid} *)

(** {1 Using the minimal definition}

    Build a {!module-type:Preface_specs.SEMIGROUPOID} using
    {!module-type:Preface_specs.Semigroupoid.WITH_COMPOSE}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_compose (Req : Preface_specs.Semigroupoid.WITH_COMPOSE) :
  Preface_specs.SEMIGROUPOID with type ('a, 'b) t = ('a, 'b) Req.t

(** {1 Semigroupoid Algebra}

    Construction of {!module-type:Preface_specs.SEMIGROUPOID} by combining them. *)

(** {2 Product}

    Construct the product of two {!module-type:Preface_specs.SEMIGROUPOID}. *)

module Product (F : Preface_specs.SEMIGROUPOID) (G : Preface_specs.SEMIGROUPOID) :
  Preface_specs.SEMIGROUPOID with type ('a, 'b) t = ('a, 'b) F.t * ('a, 'b) G.t

(** {1 From other abstraction} *)

(** {2 From a Monad}

    Produces a {!module-type:Preface_specs.SEMIGROUPOID} from a
    {!module-type:Preface_specs.MONAD}. *)

module From_monad (Monad : Preface_specs.Monad.CORE) :
  Preface_specs.SEMIGROUPOID with type ('a, 'b) t = 'a -> 'b Monad.t

(** {1 Manual construction}

    Advanced way to build a {!module-type:Preface_specs.SEMIGROUPOID},
    constructing and assembling a component-by-component of
    {!module-type:Preface_specs.SEMIGROUPOID}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Semigroupoid.CORE)
    (Operation : Preface_specs.Semigroupoid.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t)
    (Infix : Preface_specs.Semigroupoid.INFIX
               with type ('a, 'b) t = ('a, 'b) Operation.t) :
  Preface_specs.SEMIGROUPOID with type ('a, 'b) t = ('a, 'b) Infix.t

(** {2 Building Core} *)

module Core (Req : Preface_specs.Semigroupoid.WITH_COMPOSE) :
  Preface_specs.Semigroupoid.CORE with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Semigroupoid.CORE) :
  Preface_specs.Semigroupoid.OPERATION with type ('a, 'b) t = ('a, 'b) Core.t

(** {2 Deriving Infix} *)

module Infix (Core : Preface_specs.Semigroupoid.CORE) :
  Preface_specs.Semigroupoid.INFIX with type ('a, 'b) t = ('a, 'b) Core.t
