(** Building a {!module:Preface_specs.Applicative} *)

(** {1 Using the minimal definition} *)

(** {2 Using pure and apply}

    Build a {!module-type:Preface_specs.APPLICATIVE} using
    {!module-type:Preface_specs.Applicative.WITH_APPLY}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_pure_and_apply (Req : Preface_specs.Applicative.WITH_PURE_AND_APPLY) :
  Preface_specs.APPLICATIVE with type 'a t = 'a Req.t

(** {2 Using pure, map and product}

    Build a {!module-type:Preface_specs.APPLICATIVE} using
    {!module-type:Preface_specs.Applicative.WITH_MAP_AND_PRODUCT}.

    Other standard method, using the minimal definition of an alt to derive its
    full API. *)

module Via_pure_map_and_product
    (Req : Preface_specs.Applicative.WITH_PURE_MAP_AND_PRODUCT) :
  Preface_specs.APPLICATIVE with type 'a t = 'a Req.t

(** {2 Using pure and lift2}

    Build a {!module-type:Preface_specs.APPLICATIVE} using
    {!module-type:Preface_specs.Applicative.WITH_LIFT2}.

    Other standard method, using the minimal definition of an alt to derive its
    full API. *)

module Via_pure_and_lift2 (Req : Preface_specs.Applicative.WITH_PURE_AND_LIFT2) :
  Preface_specs.APPLICATIVE with type 'a t = 'a Req.t

(** {2 Over an apply}

    Build a {!module-type:Preface_specs.APPLICATIVE} over an
    {!module-type:Preface_specs.APPLY}.

    If you already have an Apply, you can enrich it by passing only [pure]. *)

module Over_apply
    (Apply : Preface_specs.APPLY)
    (Req : Preface_specs.Applicative.WITH_PURE with type 'a t = 'a Apply.t) :
  Preface_specs.APPLICATIVE with type 'a t = 'a Req.t

(** {1 Applicative Algebra}

    Construction of {!module-type:Preface_specs.APPLICATIVE} by combining them. *)

(** {2 Composition}

    Right-to-left composition of {!module-type:Preface_specs.APPLICATIVE}.*)

module Composition
    (F : Preface_specs.APPLICATIVE)
    (G : Preface_specs.APPLICATIVE) :
  Preface_specs.APPLICATIVE with type 'a t = 'a G.t F.t

(** {2 Product}

    Construct the product of two {!module-type:Preface_specs.APPLICATIVE}. *)

module Product (F : Preface_specs.APPLICATIVE) (G : Preface_specs.APPLICATIVE) :
  Preface_specs.APPLICATIVE with type 'a t = 'a F.t * 'a G.t

(** {1 From other abstraction} *)

(** {2 From a Monad}

    Produces a {!module-type:Preface_specs.APPLICATIVE} from a
    {!module-type:Preface_specs.MONAD}. *)

module From_monad (Monad : Preface_specs.MONAD) :
  Preface_specs.APPLICATIVE with type 'a t = 'a Monad.t

(** {2 From an Alternative}

    Produces a {!module-type:Preface_specs.APPLICATIVE} from an
    {!module-type:Preface_specs.ALTERNATIVE}. *)

module From_alternative (Alternative : Preface_specs.ALTERNATIVE) :
  Preface_specs.APPLICATIVE with type 'a t = 'a Alternative.t

(** {2 From an Arrow}

    Produces a {!module-type:Preface_specs.APPLICATIVE} from an
    {!module-type:Preface_specs.ARROW}. *)

module From_arrow (A : Preface_specs.ARROW) :
  Preface_specs.APPLICATIVE with type 'a t = (unit, 'a) A.t

(** {2 From a Monoid}

    Produces a {!module-type:Preface_specs.APPLICATIVE} from a
    {!module-type:Preface_specs.MONOID}. This Applicative is called [Const] or a
    phantom monoid. *)

module Const (M : Preface_specs.Monoid.CORE) : sig
  type 'a t = Const of M.t

  include Preface_specs.APPLICATIVE with type 'a t := 'a t

  val get : 'a t -> M.t
  (** Retrieve the [Monoid] value from the [Const]. *)
end

(** {1 To other abstraction} *)

(** {2 To an Indexed Alt} *)

module Index (F : Preface_specs.APPLICATIVE) :
  Preface_specs.INDEXED_APPLICATIVE with type ('a, 'index) t = 'a F.t

(** {1 Manual construction}

    Advanced way to build an {!module-type:Preface_specs.APPLICATIVE},
    constructing and assembling a component-by-component of
    {!module-type:Preface_specs.APPLICATIVE}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Applicative.CORE)
    (Operation : Preface_specs.Applicative.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Applicative.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Applicative.SYNTAX with type 'a t = 'a Core.t) :
  Preface_specs.APPLICATIVE with type 'a t = 'a Core.t

(** {2 Building Core} *)

module Core_via_pure_map_and_product
    (Req : Preface_specs.Applicative.WITH_PURE_MAP_AND_PRODUCT) :
  Preface_specs.Applicative.CORE with type 'a t = 'a Req.t

module Core_via_pure_and_apply
    (Req : Preface_specs.Applicative.WITH_PURE_AND_APPLY) :
  Preface_specs.Applicative.CORE with type 'a t = 'a Req.t

module Core_via_pure_and_lift2
    (Req : Preface_specs.Applicative.WITH_PURE_AND_LIFT2) :
  Preface_specs.Applicative.CORE with type 'a t = 'a Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Applicative.CORE) :
  Preface_specs.Applicative.OPERATION with type 'a t = 'a Core.t

(** {2 Deriving Syntax} *)

module Syntax (Core : Preface_specs.Applicative.CORE) :
  Preface_specs.Applicative.SYNTAX with type 'a t = 'a Core.t

(** {2 Deriving Infix} *)

module Infix
    (Core : Preface_specs.Applicative.CORE)
    (Operation : Preface_specs.Applicative.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Applicative.INFIX with type 'a t = 'a Core.t
