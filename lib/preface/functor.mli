(** Modules for building [Functor] modules. *)

(** {1 Internal construction of a [Functor] module} *)

module Make_via_map (Core : Preface_specs.Functor.CORE) :
  Preface_specs.FUNCTOR with type 'a t = 'a Core.t
(** Incarnation of a [Functor] for an ['a t] with standard
    Requirements ([map]). 
*)

module Make
    (Core : Preface_specs.Functor.CORE)
    (Operation : Preface_specs.Functor.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Functor.INFIX with type 'a t = 'a Core.t) :
  Preface_specs.FUNCTOR with type 'a t = 'a Core.t
(** Incarnation of a [Functor] for an ['a t] using each components of 
    a [Functor].
*)

(** {1 Internal construction of a [Monad] module} *)

module Make_operation (Core : Preface_specs.Functor.CORE) :
  Preface_specs.Functor.OPERATION with type 'a t = 'a Core.t
(** Incarnation of a [Functor.Operation] for an ['a t] with standard
    Requirements ([map]). 
*)

module Make_infix
    (Core : Preface_specs.Functor.CORE)
    (Operation : Preface_specs.Functor.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Functor.INFIX with type 'a t = 'a Core.t
(** Incarnation of a [Functor.Infix] for an ['a t] with functional API of 
    a [Functor]. 
*)
