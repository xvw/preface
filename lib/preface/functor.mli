(** Modules for building [Functor] modules. *)

(** {1 Internal construction of a [Functor] module} *)

module Make_via_map (Core : Specs.Functor.CORE) :
  Specs.FUNCTOR with type 'a t = 'a Core.t
(** Incarnation of a [Functor] for an ['a t] with standard
    Requirements ([map]). 
*)

module Make
    (Core : Specs.Functor.CORE)
    (Operation : Specs.Functor.OPERATION with type 'a t = 'a Core.t)
    (Infix : Specs.Functor.INFIX with type 'a t = 'a Core.t) :
  Specs.FUNCTOR with type 'a t = 'a Core.t
(** Incarnation of a [Functor] for an ['a t] using each components of 
    a [Functor].
*)

(** {1 Internal construction of a [Monad] module} *)

module Make_operation (Core : Specs.Functor.CORE) :
  Specs.Functor.OPERATION with type 'a t = 'a Core.t
(** Incarnation of a [Functor.Operation] for an ['a t] with standard
    Requirements ([map]). 
*)

module Make_infix
    (Core : Specs.Functor.CORE)
    (Operation : Specs.Functor.OPERATION with type 'a t = 'a Core.t) :
  Specs.Functor.INFIX with type 'a t = 'a Core.t
(** Incarnation of a [Functor.Infix] for an ['a t] with functional API of 
    a [Functor]. 
*)
