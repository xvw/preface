(** Modules for building [Selective] modules. *)

(** {1 Documentation} *)

(** {2 Construction}

    Standard way to build [Selective Functor]. *)

(** Incarnation of a [Selective] over an [Applicative] using [select]. *)
module Over_applicative_via_select
    (Applicative : Preface_specs.APPLICATIVE)
    (Req : Preface_specs.Selective.WITH_SELECT with type 'a t = 'a Applicative.t) :
  Preface_specs.SELECTIVE with type 'a t = 'a Req.t

(** Incarnation of a [Selective] over an [Applicative] using branch. *)
module Over_applicative_via_branch
    (Applicative : Preface_specs.APPLICATIVE)
    (Req : Preface_specs.Selective.WITH_BRANCH with type 'a t = 'a Applicative.t) :
  Preface_specs.SELECTIVE with type 'a t = 'a Req.t

(** Incarnation of a [Selective] over a [Functor] using [select] and [pure]. *)
module Over_functor_via_select
    (Functor : Preface_specs.Functor.CORE)
    (Req : Preface_specs.Selective.WITH_PURE_AND_SELECT
             with type 'a t = 'a Functor.t) :
  Preface_specs.SELECTIVE with type 'a t = 'a Req.t

(** Incarnation of a [Selective] over a [Functor] using [branch] and [pure]. *)
module Over_functor_via_branch
    (Functor : Preface_specs.Functor.CORE)
    (Req : Preface_specs.Selective.WITH_PURE_AND_BRANCH
             with type 'a t = 'a Functor.t) :
  Preface_specs.SELECTIVE with type 'a t = 'a Req.t

(** Incarnation of an [Selective] using an [Arrow_choice] via [Arrow Monad]
    encoding.*)
module From_arrow_choice (A : Preface_specs.ARROW_CHOICE) :
  Preface_specs.SELECTIVE with type 'a t = (unit, 'a) A.t

(** Incarnation of a [Selective] over a [Monoid] using [Const] (as
    [Over approximation]).*)
module Const (M : Preface_specs.Monoid.CORE) : sig
  type 'a t = Const of M.t

  include Preface_specs.SELECTIVE with type 'a t := 'a t

  val get : 'a t -> M.t
  (** Retreive the [Monoid] value from the [Const]. *)
end

(** {2 Selective composition}

    Some tools for composition between selectives. *)

(** Right-to-left composition of selectives with applicatives.*)
module Composition (F : Preface_specs.APPLICATIVE) (G : Preface_specs.SELECTIVE) :
  Preface_specs.SELECTIVE with type 'a t = 'a G.t F.t

(** Product of two Selectives *)
module Product (F : Preface_specs.SELECTIVE) (G : Preface_specs.SELECTIVE) :
  Preface_specs.SELECTIVE with type 'a t = 'a F.t * 'a G.t

(** {2 Manual construction}

    Advanced way to build an [Selective Functor], constructing and assembling a
    component-by-component a selective functor. (In order to provide your own
    implementation for some features.) *)

(** Incarnation of a [Selective] using each components of a [Selective]. *)
module Via
    (Core : Preface_specs.Selective.CORE)
    (Operation : Preface_specs.Selective.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Selective.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Selective.SYNTAX with type 'a t = 'a Core.t) :
  Preface_specs.SELECTIVE with type 'a t = 'a Core.t

(** Incarnation of a [Selective.Core] over a [Functor] via [select] and [pure]. *)
module Core_over_functor_via_select
    (Functor : Preface_specs.Functor.CORE)
    (Req : Preface_specs.Selective.WITH_PURE_AND_SELECT
             with type 'a t = 'a Functor.t) :
  Preface_specs.Selective.CORE with type 'a t = 'a Req.t

(** Incarnation of a [Selective.Core] over a [Functor] via [branch] and [pure]. *)
module Core_over_functor_via_branch
    (Functor : Preface_specs.Functor.CORE)
    (Req : Preface_specs.Selective.WITH_PURE_AND_BRANCH
             with type 'a t = 'a Functor.t) :
  Preface_specs.Selective.CORE with type 'a t = 'a Req.t

(** Incarnation of a [Selective.Core] over an [Applicative] using [select]. *)
module Core_over_applicative_via_select
    (Applicative : Preface_specs.APPLICATIVE)
    (Req : Preface_specs.Selective.WITH_SELECT with type 'a t = 'a Applicative.t) :
  Preface_specs.Selective.CORE with type 'a t = 'a Req.t

(** Incarnation of a [Selective.Core] over an [Applicative] using [branch]. *)
module Core_over_applicative_via_branch
    (Applicative : Preface_specs.APPLICATIVE)
    (Req : Preface_specs.Selective.WITH_BRANCH with type 'a t = 'a Applicative.t) :
  Preface_specs.Selective.CORE with type 'a t = 'a Req.t

(** Incarnation of [Selective.Operation] using [Selective.Core]. *)
module Operation (Core : Preface_specs.Selective.CORE) :
  Preface_specs.Selective.OPERATION with type 'a t = 'a Core.t

(** Incarnation of [Selective.Infix] using [Selective.Core] and
    [Selective.Operation]. *)
module Infix
    (Core : Preface_specs.Selective.CORE)
    (Operation : Preface_specs.Selective.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Selective.INFIX with type 'a t = 'a Core.t

(** Incarnation of [Selective.Syntax] using [Selective.Core]. *)
module Syntax (Core : Preface_specs.Selective.CORE) :
  Preface_specs.Selective.SYNTAX with type 'a t = 'a Core.t

(** Incarnation of [Select] using a [Monad]. *)
module Select_from_monad (Monad : Preface_specs.MONAD) :
  Preface_specs.Selective.WITH_SELECT with type 'a t = 'a Monad.t
