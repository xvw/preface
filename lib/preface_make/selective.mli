(** Building a {!module:Preface_specs.Selective} *)

(** {1 Using the minimal definition} *)

(** {2 Over an Applicative using select}

    Build a {!module-type:Preface_specs.SELECTIVE} using
    {!module-type:Preface_specs.Selective.WITH_SELECT} on top of an
    {!module-type:Preface_specs.APPLICATIVE}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_applicative_via_select
    (Applicative : Preface_specs.APPLICATIVE)
    (Req : Preface_specs.Selective.WITH_SELECT with type 'a t = 'a Applicative.t) :
  Preface_specs.SELECTIVE with type 'a t = 'a Req.t

(** {2 Over an Applicative using branch}

    Build a {!module-type:Preface_specs.SELECTIVE} using
    {!module-type:Preface_specs.Selective.WITH_BRANCH} on top of an
    {!module-type:Preface_specs.APPLICATIVE}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_applicative_via_branch
    (Applicative : Preface_specs.APPLICATIVE)
    (Req : Preface_specs.Selective.WITH_BRANCH with type 'a t = 'a Applicative.t) :
  Preface_specs.SELECTIVE with type 'a t = 'a Req.t

(** {2 Over a Functor using select}

    Build a {!module-type:Preface_specs.SELECTIVE} using
    {!module-type:Preface_specs.Selective.WITH_SELECT} on top of an
    {!module-type:Preface_specs.FUNCTOR}.

    Standard method, using the minimal definition of an alt to derive its full
    API (including the Applicative API). *)

module Over_functor_via_select
    (Functor : Preface_specs.Functor.CORE)
    (Req : Preface_specs.Selective.WITH_PURE_AND_SELECT
             with type 'a t = 'a Functor.t) :
  Preface_specs.SELECTIVE with type 'a t = 'a Req.t

(** {2 Over a Functor using branch}

    Build a {!module-type:Preface_specs.SELECTIVE} using
    {!module-type:Preface_specs.Selective.WITH_BRANCH} on top of an
    {!module-type:Preface_specs.FUNCTOR}.

    Standard method, using the minimal definition of an alt to derive its full
    API (including the Applicative API). *)

module Over_functor_via_branch
    (Functor : Preface_specs.Functor.CORE)
    (Req : Preface_specs.Selective.WITH_PURE_AND_BRANCH
             with type 'a t = 'a Functor.t) :
  Preface_specs.SELECTIVE with type 'a t = 'a Req.t

(** {1 Selective Algebra}

    Construction of {!module-type:Preface_specs.SELECTIVE} by combining them. *)

(** {2 Composition}

    Right-to-left composition of {!module-type:Preface_specs.APPLICATIVE} with
    {!module-type:Preface_specs.SELECTIVE}.*)

module Composition (F : Preface_specs.APPLICATIVE) (G : Preface_specs.SELECTIVE) :
  Preface_specs.SELECTIVE with type 'a t = 'a G.t F.t

(** {2 Product}

    Construct the product of two {!module-type:Preface_specs.SELECTIVE}. *)

module Product (F : Preface_specs.SELECTIVE) (G : Preface_specs.SELECTIVE) :
  Preface_specs.SELECTIVE with type 'a t = 'a F.t * 'a G.t

(** {1 From other abstraction} *)

(** {2 From an Arrow choice}

    Produces a {!module-type:Preface_specs.SELECTIVE} from an
    {!module-type:Preface_specs.ARROW_CHOICE}. *)

module From_arrow_choice (A : Preface_specs.ARROW_CHOICE) :
  Preface_specs.SELECTIVE with type 'a t = (unit, 'a) A.t

(** {2 From a Monoid}

    Produces a {!module-type:Preface_specs.SELECTIVE} from a
    {!module-type:Preface_specs.MONOID}. This Selective is called [Const] or a
    phantom monoid. *)

module Const (M : Preface_specs.Monoid.CORE) : sig
  type 'a t = Const of M.t

  include Preface_specs.SELECTIVE with type 'a t := 'a t

  val get : 'a t -> M.t
  (** Retrieve the [Monoid] value from the [Const]. *)
end

(** {1 To other abstraction} *)

(** {2 To an Indexed Applicative} *)

module Index (F : Preface_specs.SELECTIVE) :
  Preface_specs.INDEXED_SELECTIVE with type ('a, 'index) t = 'a F.t

(** {1 Manual construction}

    Advanced way to build a {!module-type:Preface_specs.SELECTIVE}, constructing
    and assembling a component-by-component of
    {!module-type:Preface_specs.SELECTIVE}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Selective.CORE)
    (Operation : Preface_specs.Selective.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Selective.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Selective.SYNTAX with type 'a t = 'a Core.t) :
  Preface_specs.SELECTIVE with type 'a t = 'a Core.t

(** {2 Building Core} *)

module Core_over_functor_via_select
    (Functor : Preface_specs.Functor.CORE)
    (Req : Preface_specs.Selective.WITH_PURE_AND_SELECT
             with type 'a t = 'a Functor.t) :
  Preface_specs.Selective.CORE with type 'a t = 'a Req.t

module Core_over_functor_via_branch
    (Functor : Preface_specs.Functor.CORE)
    (Req : Preface_specs.Selective.WITH_PURE_AND_BRANCH
             with type 'a t = 'a Functor.t) :
  Preface_specs.Selective.CORE with type 'a t = 'a Req.t

module Core_over_applicative_via_select
    (Applicative : Preface_specs.Applicative.CORE)
    (Req : Preface_specs.Selective.WITH_SELECT with type 'a t = 'a Applicative.t) :
  Preface_specs.Selective.CORE with type 'a t = 'a Req.t

module Core_over_applicative_via_branch
    (Applicative : Preface_specs.Applicative.CORE)
    (Req : Preface_specs.Selective.WITH_BRANCH with type 'a t = 'a Applicative.t) :
  Preface_specs.Selective.CORE with type 'a t = 'a Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Selective.CORE) :
  Preface_specs.Selective.OPERATION with type 'a t = 'a Core.t

(** {2 Deriving Infix} *)

module Infix
    (Core : Preface_specs.Selective.CORE)
    (Operation : Preface_specs.Selective.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Selective.INFIX with type 'a t = 'a Core.t

(** {2 Deriving Syntax} *)

module Syntax (Core : Preface_specs.Selective.CORE) :
  Preface_specs.Selective.SYNTAX with type 'a t = 'a Core.t

(** {2 Deriving Select from a Monad} *)

module Select_from_monad (Monad : Preface_specs.MONAD) :
  Preface_specs.Selective.WITH_SELECT with type 'a t = 'a Monad.t
