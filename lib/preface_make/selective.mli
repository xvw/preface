(** Modules for building [Selective] modules. *)

(** {1 Documentation} *)

(** {2 Construction}

    Standard way to build [Selective Functor]. *)

(** Incarnation of a [Selective] over an [Applicative]. *)
module Over_applicative
    (Applicative : Preface_specs.APPLICATIVE)
    (Select : Preface_specs.Selective.CORE_WITH_SELECT
                with type 'a t = 'a Applicative.t) :
  Preface_specs.SELECTIVE with type 'a t = 'a Select.t

(** Incarnation of a [Selective] over a [Functor]. *)
module Over_functor
    (Functor : Preface_specs.Functor.CORE)
    (Select : Preface_specs.Selective.CORE_WITH_SELECT
                with type 'a t = 'a Functor.t) :
  Preface_specs.SELECTIVE with type 'a t = 'a Select.t

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

(** Incarnation of a [Selective.Core] over a [Functor]. *)
module Core_over_functor
    (Functor : Preface_specs.Functor.CORE)
    (Select : Preface_specs.Selective.CORE_WITH_SELECT
                with type 'a t = 'a Functor.t) :
  Preface_specs.Selective.CORE with type 'a t = 'a Functor.t

(** Incarnation of a [Selective.Core] over an [Applicative]. *)
module Core_over_applicative
    (Applicative : Preface_specs.APPLICATIVE)
    (Select : Preface_specs.Selective.CORE_WITH_SELECT
                with type 'a t = 'a Applicative.t) :
  Preface_specs.Selective.CORE with type 'a t = 'a Applicative.t

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
  Preface_specs.Selective.CORE_WITH_SELECT with type 'a t = 'a Monad.t
