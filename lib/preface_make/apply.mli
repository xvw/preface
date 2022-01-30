(** Building a {!module:Preface_specs.APPLY} *)

(** {1 Using the minimal definition} *)

(** {2 Using map and apply}

    Build a {!module-type:Preface_specs.APPLY} using
    {!module-type:Preface_specs.APPLY.WITH_MAP_AND_APPLY}.

    Other standard method, using the minimal definition of an alt to derive its
    full API. *)

module Via_map_and_apply (Req : Preface_specs.Apply.WITH_MAP_AND_APPLY) :
  Preface_specs.APPLY with type 'a t = 'a Req.t

(** {2 Using apply over functor}

    Build a {!module-type:Preface_specs.APPLY} using
    {!module-type:Preface_specs.Functor.WITH_MAP} and
    {!module-type:Preface_specs.APPLY.WITH_APPLY}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_functor_via_apply
    (Functor : Preface_specs.Functor.WITH_MAP)
    (Req : Preface_specs.Apply.WITH_APPLY with type 'a t = 'a Functor.t) :
  Preface_specs.APPLY with type 'a t = 'a Req.t

(** {2 Using map and product}

    Build a {!module-type:Preface_specs.APPLY} using
    {!module-type:Preface_specs.APPLY.WITH_MAP_AND_PRODUCT}.

    Other standard method, using the minimal definition of an alt to derive its
    full API. *)

module Via_map_and_product (Req : Preface_specs.Apply.WITH_MAP_AND_PRODUCT) :
  Preface_specs.APPLY with type 'a t = 'a Req.t

(** {2 Using product over functor}

    Build a {!module-type:Preface_specs.APPLY} using
    {!module-type:Preface_specs.Functor.WITH_MAP} and
    {!module-type:Preface_specs.APPLY.WITH_PRODUCT}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_functor_via_product
    (Functor : Preface_specs.Functor.WITH_MAP)
    (Req : Preface_specs.Apply.WITH_PRODUCT with type 'a t = 'a Functor.t) :
  Preface_specs.APPLY with type 'a t = 'a Req.t

(** {2 Using map and lift2}

    Build a {!module-type:Preface_specs.APPLY} using
    {!module-type:Preface_specs.APPLY.WITH_LIFT2}.

    Other standard method, using the minimal definition of an alt to derive its
    full API. *)

module Via_map_and_lift2 (Req : Preface_specs.Apply.WITH_MAP_AND_LIFT2) :
  Preface_specs.APPLY with type 'a t = 'a Req.t

(** {2 Using lift2 over functor}

    Build a {!module-type:Preface_specs.APPLY} using
    {!module-type:Preface_specs.Functor.WITH_MAP} and
    {!module-type:Preface_specs.APPLY.WITH_LIFT2}.

    Other standard method, using the minimal definition of an alt to derive its
    full API. *)

module Over_functor_via_lift2
    (Functor : Preface_specs.Functor.WITH_MAP)
    (Req : Preface_specs.Apply.WITH_LIFT2 with type 'a t = 'a Functor.t) :
  Preface_specs.APPLY with type 'a t = 'a Req.t

(** {1 APPLY Algebra}

    Construction of {!module-type:Preface_specs.APPLY} by combining them. *)

(** {2 Composition}

    Right-to-left composition of {!module-type:Preface_specs.APPLY}.*)

module Composition (F : Preface_specs.APPLY) (G : Preface_specs.APPLY) :
  Preface_specs.APPLY with type 'a t = 'a G.t F.t

(** {2 Product}

    Construct the product of two {!module-type:Preface_specs.APPLY}. *)

module Product (F : Preface_specs.APPLY) (G : Preface_specs.APPLY) :
  Preface_specs.APPLY with type 'a t = 'a F.t * 'a G.t

(** {1 From other abstraction} *)

(** {2 From a Monad}

    Produces a {!module-type:Preface_specs.APPLY} from a
    {!module-type:Preface_specs.MONAD}. *)

module From_monad (Monad : Preface_specs.MONAD) :
  Preface_specs.APPLY with type 'a t = 'a Monad.t

(** {2 From an Applicative}

    Produces a {!module-type:Preface_specs.APPLY} from a
    {!module-type:Preface_specs.MONAD}. *)

module From_applicative (Applicative : Preface_specs.APPLICATIVE) :
  Preface_specs.APPLY with type 'a t = 'a Applicative.t

(** {2 From an Alternative}

    Produces a {!module-type:Preface_specs.APPLY} from an
    {!module-type:Preface_specs.ALTERNATIVE}. *)

module From_alternative (Alternative : Preface_specs.ALTERNATIVE) :
  Preface_specs.APPLY with type 'a t = 'a Alternative.t

(** {2 From an Arrow}

    Produces a {!module-type:Preface_specs.APPLY} from an
    {!module-type:Preface_specs.ARROW}. *)

module From_arrow (A : Preface_specs.ARROW) :
  Preface_specs.APPLY with type 'a t = (unit, 'a) A.t

(** {2 From a Monoid}

    Produces a {!module-type:Preface_specs.APPLY} from a
    {!module-type:Preface_specs.MONOID}. This APPLY is called [Const] or a
    phantom monoid. *)

module Const (M : Preface_specs.Monoid.CORE) : sig
  type 'a t = Const of M.t

  include Preface_specs.APPLY with type 'a t := 'a t

  val get : 'a t -> M.t
  (** Retrieve the [Monoid] value from the [Const]. *)
end

(** {1 Manual construction}

    Advanced way to build an {!module-type:Preface_specs.APPLY}, constructing
    and assembling a component-by-component of
    {!module-type:Preface_specs.APPLY}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Apply.CORE)
    (Operation : Preface_specs.Apply.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Apply.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Apply.SYNTAX with type 'a t = 'a Core.t) :
  Preface_specs.APPLY with type 'a t = 'a Core.t

(** {2 Building Core} *)

module Core_via_map_and_apply (Req : Preface_specs.Apply.WITH_MAP_AND_APPLY) :
  Preface_specs.Apply.CORE with type 'a t = 'a Req.t

module Core_via_map_and_product (Req : Preface_specs.Apply.WITH_MAP_AND_PRODUCT) :
  Preface_specs.Apply.CORE with type 'a t = 'a Req.t

module Core_via_map_and_lift2 (Req : Preface_specs.Apply.WITH_MAP_AND_LIFT2) :
  Preface_specs.Apply.CORE with type 'a t = 'a Req.t

module Core_over_functor_via_apply
    (Functor : Preface_specs.Functor.WITH_MAP)
    (Req : Preface_specs.Apply.WITH_APPLY with type 'a t = 'a Functor.t) :
  Preface_specs.Apply.CORE with type 'a t = 'a Req.t

module Core_over_functor_via_product
    (Functor : Preface_specs.Functor.WITH_MAP)
    (Req : Preface_specs.Apply.WITH_PRODUCT with type 'a t = 'a Functor.t) :
  Preface_specs.Apply.CORE with type 'a t = 'a Req.t

module Core_over_functor_via_lift2
    (Functor : Preface_specs.Functor.WITH_MAP)
    (Req : Preface_specs.Apply.WITH_LIFT2 with type 'a t = 'a Functor.t) :
  Preface_specs.Apply.CORE with type 'a t = 'a Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Apply.CORE) :
  Preface_specs.Apply.OPERATION with type 'a t = 'a Core.t

(** {2 Deriving Syntax} *)

module Syntax (Core : Preface_specs.Apply.CORE) :
  Preface_specs.Apply.SYNTAX with type 'a t = 'a Core.t

(** {2 Deriving Infix} *)

module Infix
    (Core : Preface_specs.Apply.CORE)
    (Operation : Preface_specs.Apply.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Apply.INFIX with type 'a t = 'a Core.t
