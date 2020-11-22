(** Modules for building [Selective] modules. *)

(** {1 Construction of a [Selective] module with [Either] from [Preface_core]} *)

module Over_applicative
    (Applicative : Preface_specs.APPLICATIVE)
    (Select : Preface_specs.Selective.CORE_WITH_SELECT
                with type 'a t = 'a Applicative.t
                 and type ('a, 'b) either = ('a, 'b) Preface_core.Either.t) :
  Preface_specs.SELECTIVE
    with type 'a t = 'a Select.t
     and type ('a, 'b) either = ('a, 'b) Preface_core.Either.t

module Over_functor
    (Functor : Preface_specs.Functor.CORE)
    (Select : Preface_specs.Selective.CORE_WITH_SELECT
                with type 'a t = 'a Functor.t
                 and type ('a, 'b) either = ('a, 'b) Preface_core.Either.t) :
  Preface_specs.SELECTIVE
    with type 'a t = 'a Select.t
     and type ('a, 'b) either = ('a, 'b) Preface_core.Either.t

(** {1 Construction of a [Selective] module giving [Eihter]} *)

module Over_applicative_and_either
    (Either : Preface_core.Requirements.EITHER)
    (Applicative : Preface_specs.APPLICATIVE)
    (Select : Preface_specs.Selective.CORE_WITH_SELECT
                with type 'a t = 'a Applicative.t
                 and type ('a, 'b) either = ('a, 'b) Either.t) :
  Preface_specs.SELECTIVE
    with type 'a t = 'a Select.t
     and type ('a, 'b) either = ('a, 'b) Either.t

module Over_functor_and_either
    (Either : Preface_core.Requirements.EITHER)
    (Functor : Preface_specs.Functor.CORE)
    (Select : Preface_specs.Selective.CORE_WITH_SELECT
                with type 'a t = 'a Functor.t
                 and type ('a, 'b) either = ('a, 'b) Either.t) :
  Preface_specs.SELECTIVE
    with type 'a t = 'a Select.t
     and type ('a, 'b) either = ('a, 'b) Either.t

module Over_either
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

(** {2 With [Either] from [Preface_core]} *)

module Core_over_applicative
    (Applicative : Preface_specs.APPLICATIVE)
    (Select : Preface_specs.Selective.CORE_WITH_SELECT
                with type 'a t = 'a Applicative.t
                 and type ('a, 'b) either = ('a, 'b) Preface_core.Either.t) :
  Preface_specs.Selective.CORE
    with type 'a t = 'a Applicative.t
     and type ('a, 'b) either = ('a, 'b) Preface_core.Either.t

module Core_over_functor
    (Functor : Preface_specs.Functor.CORE)
    (Select : Preface_specs.Selective.CORE_WITH_SELECT
                with type 'a t = 'a Functor.t
                 and type ('a, 'b) either = ('a, 'b) Preface_core.Either.t) :
  Preface_specs.Selective.CORE
    with type 'a t = 'a Functor.t
     and type ('a, 'b) either = ('a, 'b) Preface_core.Either.t

module Operation_over
    (Core : Preface_specs.Selective.CORE
              with type ('a, 'b) either = ('a, 'b) Preface_core.Either.t) :
  Preface_specs.Selective.OPERATION
    with type 'a t = 'a Core.t
     and type ('a, 'b) either = ('a, 'b) Preface_core.Either.t

module Infix_over
    (Core : Preface_specs.Selective.CORE
              with type ('a, 'b) either = ('a, 'b) Preface_core.Either.t)
    (Operation : Preface_specs.Selective.OPERATION
                   with type 'a t = 'a Core.t
                    and type ('a, 'b) either = ('a, 'b) Preface_core.Either.t) :
  Preface_specs.Selective.INFIX
    with type 'a t = 'a Core.t
     and type ('a, 'b) either = ('a, 'b) Preface_core.Either.t

module Syntax_over
    (Core : Preface_specs.Selective.CORE
              with type ('a, 'b) either = ('a, 'b) Preface_core.Either.t) :
  Preface_specs.Selective.SYNTAX with type 'a t = 'a Core.t

module Select_from_monad (Monad : Preface_specs.MONAD) :
  Preface_specs.Selective.CORE_WITH_SELECT
    with type 'a t = 'a Monad.t
     and type ('a, 'b) either = ('a, 'b) Preface_core.Either.t

(** {2 Giving [Either]} *)

module Core_over_functor_and_either
    (Either : Preface_core.Requirements.EITHER)
    (Functor : Preface_specs.Functor.CORE)
    (Select : Preface_specs.Selective.CORE_WITH_SELECT
                with type 'a t = 'a Functor.t
                 and type ('a, 'b) either = ('a, 'b) Either.t) :
  Preface_specs.Selective.CORE
    with type 'a t = 'a Functor.t
     and type ('a, 'b) either = ('a, 'b) Either.t

module Core_over_applicative_and_either
    (Either : Preface_core.Requirements.EITHER)
    (Applicative : Preface_specs.APPLICATIVE)
    (Select : Preface_specs.Selective.CORE_WITH_SELECT
                with type 'a t = 'a Applicative.t
                 and type ('a, 'b) either = ('a, 'b) Either.t) :
  Preface_specs.Selective.CORE
    with type 'a t = 'a Applicative.t
     and type ('a, 'b) either = ('a, 'b) Either.t

module Operation_over_either
    (Either : Preface_core.Requirements.EITHER)
    (Core : Preface_specs.Selective.CORE
              with type ('a, 'b) either = ('a, 'b) Either.t) :
  Preface_specs.Selective.OPERATION
    with type 'a t = 'a Core.t
     and type ('a, 'b) either = ('a, 'b) Either.t

module Infix_over_either
    (Either : Preface_core.Requirements.EITHER)
    (Core : Preface_specs.Selective.CORE
              with type ('a, 'b) either = ('a, 'b) Either.t)
    (Operation : Preface_specs.Selective.OPERATION
                   with type 'a t = 'a Core.t
                    and type ('a, 'b) either = ('a, 'b) Either.t) :
  Preface_specs.Selective.INFIX
    with type 'a t = 'a Core.t
     and type ('a, 'b) either = ('a, 'b) Either.t

module Syntax_over_either
    (Either : Preface_core.Requirements.EITHER)
    (Core : Preface_specs.Selective.CORE
              with type ('a, 'b) either = ('a, 'b) Either.t) :
  Preface_specs.Selective.SYNTAX with type 'a t = 'a Core.t

module Select_from_monad_and_either
    (Either : Preface_core.Requirements.EITHER)
    (Monad : Preface_specs.MONAD) :
  Preface_specs.Selective.CORE_WITH_SELECT
    with type 'a t = 'a Monad.t
     and type ('a, 'b) either = ('a, 'b) Either.t
