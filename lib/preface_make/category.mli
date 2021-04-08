(** Building a {!module:Preface_specs.Category} *)

(** {1 Using the minimal definition}

    Build a {!module-type:Preface_specs.CATEGORY} using
    {!module-type:Preface_specs.Category.WITH_ID_AND_COMPOSE}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_id_and_compose (Req : Preface_specs.Category.WITH_ID_AND_COMPOSE) :
  Preface_specs.CATEGORY with type ('a, 'b) t = ('a, 'b) Req.t

(** {1 Category Algebra}

    Construction of {!module-type:Preface_specs.CATEGORY} by combining them. *)

(** {2 Product}

    Construct the product of two {!module-type:Preface_specs.CATEGORY}. *)

module Product (F : Preface_specs.CATEGORY) (G : Preface_specs.CATEGORY) :
  Preface_specs.CATEGORY with type ('a, 'b) t = ('a, 'b) F.t * ('a, 'b) G.t

(** {1 From other abstraction} *)

(** {2 From a Monad}

    Produces a {!module-type:Preface_specs.CATEGORY} from a
    {!module-type:Preface_specs.MONAD}. *)

module From_monad (Monad : Preface_specs.Monad.CORE) :
  Preface_specs.CATEGORY with type ('a, 'b) t = 'a -> 'b Monad.t

(** {1 Manual construction}

    Advanced way to build a {!module-type:Preface_specs.CATEGORY}, constructing
    and assembling a component-by-component of
    {!module-type:Preface_specs.CATEGORY}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Category.CORE)
    (Operation : Preface_specs.Category.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t)
    (Infix : Preface_specs.Category.INFIX
               with type ('a, 'b) t = ('a, 'b) Operation.t) :
  Preface_specs.CATEGORY with type ('a, 'b) t = ('a, 'b) Infix.t

(** {2 Building Core} *)

module Core (Req : Preface_specs.Category.WITH_ID_AND_COMPOSE) :
  Preface_specs.Category.CORE with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Category.CORE) :
  Preface_specs.Category.OPERATION with type ('a, 'b) t = ('a, 'b) Core.t

(** {2 Deriving Infix} *)

module Infix (Core : Preface_specs.Category.CORE) :
  Preface_specs.Category.INFIX with type ('a, 'b) t = ('a, 'b) Core.t
