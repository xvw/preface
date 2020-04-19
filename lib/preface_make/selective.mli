(** Modules for building [Selective] modules. *)

(** {1 Construction of a [Selective] module} *)

module Over_applicative
    (Either : Preface_core.Requirements.EITHER)
    (Applicative : Preface_specs.APPLICATIVE)
    (Select : Preface_specs.Selective.CORE_WITH_SELECT
                with type 'a t = 'a Applicative.t
                 and type ('a, 'b) either = ('a, 'b) Either.t) :
  Preface_specs.SELECTIVE
    with type 'a t = 'a Select.t
     and type ('a, 'b) either = ('a, 'b) Either.t

module Over_functor
    (Either : Preface_core.Requirements.EITHER)
    (Functor : Preface_specs.FUNCTOR)
    (Select : Preface_specs.Selective.CORE_WITH_SELECT
                with type 'a t = 'a Functor.t
                 and type ('a, 'b) either = ('a, 'b) Either.t) :
  Preface_specs.SELECTIVE
    with type 'a t = 'a Select.t
     and type ('a, 'b) either = ('a, 'b) Either.t

module Via
    (Either : Preface_core.Requirements.EITHER)
    (Core : Preface_specs.Selective.CORE
              with type ('a, 'b) either = ('a, 'b) Either.t)
    (Operation : Preface_specs.Selective.OPERATION
                   with type 'a t = 'a Core.t
                    and type ('a, 'b) either = ('a, 'b) Either.t)
    (Infix : Preface_specs.Selective.INFIX
               with type 'a t = 'a Core.t
                and type ('a, 'b) either = ('a, 'b) Either.t)
    (Syntax : Preface_specs.Selective.SYNTAX with type 'a t = 'a Core.t) :
  Preface_specs.SELECTIVE
    with type 'a t = 'a Core.t
     and type ('a, 'b) either = ('a, 'b) Either.t

(** {1 Internal construction of a [Selective] module} *)

module Core_via_functor
    (Either : Preface_core.Requirements.EITHER)
    (Functor : Preface_specs.FUNCTOR)
    (Select : Preface_specs.Selective.CORE_WITH_SELECT
                with type 'a t = 'a Functor.t
                 and type ('a, 'b) either = ('a, 'b) Either.t) :
  Preface_specs.Selective.CORE
    with type 'a t = 'a Functor.t
     and type ('a, 'b) either = ('a, 'b) Either.t

module Core_via_applicative
    (Either : Preface_core.Requirements.EITHER)
    (Applicative : Preface_specs.APPLICATIVE)
    (Select : Preface_specs.Selective.CORE_WITH_SELECT
                with type 'a t = 'a Applicative.t
                 and type ('a, 'b) either = ('a, 'b) Either.t) :
  Preface_specs.Selective.CORE
    with type 'a t = 'a Applicative.t
     and type ('a, 'b) either = ('a, 'b) Either.t

module Operation
    (Either : Preface_core.Requirements.EITHER)
    (Core : Preface_specs.Selective.CORE
              with type ('a, 'b) either = ('a, 'b) Either.t) :
  Preface_specs.Selective.OPERATION
    with type 'a t = 'a Core.t
     and type ('a, 'b) either = ('a, 'b) Either.t

module Infix
    (Either : Preface_core.Requirements.EITHER)
    (Core : Preface_specs.Selective.CORE
              with type ('a, 'b) either = ('a, 'b) Either.t)
    (Operation : Preface_specs.Selective.OPERATION
                   with type 'a t = 'a Core.t
                    and type ('a, 'b) either = ('a, 'b) Either.t) :
  Preface_specs.Selective.INFIX
    with type 'a t = 'a Core.t
     and type ('a, 'b) either = ('a, 'b) Either.t

module Syntax
    (Either : Preface_core.Requirements.EITHER)
    (Core : Preface_specs.Selective.CORE
              with type ('a, 'b) either = ('a, 'b) Either.t) :
  Preface_specs.Selective.SYNTAX with type 'a t = 'a Core.t
