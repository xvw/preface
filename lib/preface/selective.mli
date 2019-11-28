(** Modules for building [Selective] modules. *)

(** {1 Construction of a [Selective] module} *)

module Make_via_applicative
    (Applicative : Preface_specs.APPLICATIVE)
    (Select : Preface_specs.Selective.CORE_VIA_SELECT with type 'a t = 'a Applicative.t) :
  Preface_specs.SELECTIVE with type 'a t = 'a Select.t

module Make_via_functor
    (Functor : Preface_specs.FUNCTOR)
    (Select : Preface_specs.Selective.CORE_VIA_SELECT with type 'a t = 'a Functor.t) :
  Preface_specs.SELECTIVE with type 'a t = 'a Select.t

module Make
    (Core : Preface_specs.Selective.CORE)
    (Operation : Preface_specs.Selective.OPERATION
                   with type 'a t = 'a Core.t
                    and module Either = Core.Either)
    (Infix : Preface_specs.Selective.INFIX
               with type 'a t = 'a Core.t
                and module Either = Core.Either)
    (Syntax : Preface_specs.Selective.SYNTAX with type 'a t = 'a Core.t) :
  Preface_specs.SELECTIVE with type 'a t = 'a Core.t

(** {1 Internal construction of a [Selective] module} *)

module Make_core_via_functor
    (Functor : Preface_specs.FUNCTOR)
    (Select : Preface_specs.Selective.CORE_VIA_SELECT with type 'a t = 'a Functor.t) :
  Preface_specs.Selective.CORE
    with type 'a t = 'a Functor.t
     and module Either = Select.Either

module Make_core_via_applicative
    (Applicative : Preface_specs.APPLICATIVE)
    (Select : Preface_specs.Selective.CORE_VIA_SELECT with type 'a t = 'a Applicative.t) :
  Preface_specs.Selective.CORE
    with type 'a t = 'a Applicative.t
     and module Either = Select.Either

module Make_operation (Core : Preface_specs.Selective.CORE) :
  Preface_specs.Selective.OPERATION
    with type 'a t = 'a Core.t
     and module Either = Core.Either

module Make_infix
    (Core : Preface_specs.Selective.CORE)
    (Operation : Preface_specs.Selective.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Selective.INFIX
    with type 'a t = 'a Core.t
     and module Either = Core.Either

module Make_syntax (Core : Preface_specs.Selective.CORE) :
  Preface_specs.Selective.SYNTAX with type 'a t = 'a Core.t
