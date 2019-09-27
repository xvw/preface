(** Modules for building [Selective] modules. *)

(** {1 Construction of a [Selective] module} *)

module Make_via_applicative
    (Applicative : Specs.APPLICATIVE)
    (Select : Specs.Selective.CORE_VIA_SELECT with type 'a t = 'a Applicative.t) :
  Specs.SELECTIVE with type 'a t = 'a Select.t

module Make_via_functor
    (Functor : Specs.FUNCTOR)
    (Select : Specs.Selective.CORE_VIA_SELECT with type 'a t = 'a Functor.t) :
  Specs.SELECTIVE with type 'a t = 'a Select.t

module Make
    (Core : Specs.Selective.CORE)
    (Operation : Specs.Selective.OPERATION
                   with type 'a t = 'a Core.t
                    and module Either = Core.Either)
    (Infix : Specs.Selective.INFIX
               with type 'a t = 'a Core.t
                and module Either = Core.Either)
    (Syntax : Specs.Selective.SYNTAX with type 'a t = 'a Core.t) :
  Specs.SELECTIVE with type 'a t = 'a Core.t

(** {1 Internal construction of a [Selective] module} *)

module Make_core_via_functor
    (Functor : Specs.FUNCTOR)
    (Select : Specs.Selective.CORE_VIA_SELECT with type 'a t = 'a Functor.t) :
  Specs.Selective.CORE
    with type 'a t = 'a Functor.t
     and module Either = Select.Either

module Make_core_via_applicative
    (Applicative : Specs.APPLICATIVE)
    (Select : Specs.Selective.CORE_VIA_SELECT with type 'a t = 'a Applicative.t) :
  Specs.Selective.CORE
    with type 'a t = 'a Applicative.t
     and module Either = Select.Either

module Make_operation (Core : Specs.Selective.CORE) :
  Specs.Selective.OPERATION
    with type 'a t = 'a Core.t
     and module Either = Core.Either

module Make_infix
    (Core : Specs.Selective.CORE)
    (Operation : Specs.Selective.OPERATION with type 'a t = 'a Core.t) :
  Specs.Selective.INFIX
    with type 'a t = 'a Core.t
     and module Either = Core.Either

module Make_syntax (Core : Specs.Selective.CORE) :
  Specs.Selective.SYNTAX with type 'a t = 'a Core.t
