(** Modules for building {!Preface_specs.FUNCTOR2} modules. *)

(** {2 Construction}

    Standard way to build a [Functor2]. *)

(** Incarnation of a [Functor2] with standard requirements ([map]).*)
module Via_map (Core : Preface_specs.Functor2.CORE) :
  Preface_specs.FUNCTOR2 with type ('a, 'b) t := ('a, 'b) Core.t

(** {2 Manual construction}

    Advanced way to build a [Functor2], constructing and assembling a
    component-by-component functor. (In order to provide your own implementation
    for some features.) *)

(** Incarnation of a [Functor2] using each components of a [Functor2]. *)
module Via
    (Core : Preface_specs.Functor2.CORE)
    (Operation : Preface_specs.Functor2.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t)
    (Infix : Preface_specs.Functor2.INFIX
               with type ('a, 'b) t = ('a, 'b) Operation.t) :
  Preface_specs.FUNCTOR2 with type ('a, 'b) t := ('a, 'b) Core.t

(** Incarnation of a [Functor2.Operation] with standard Requirements ([map]). *)
module Operation (Core : Preface_specs.Functor2.CORE) :
  Preface_specs.Functor2.OPERATION with type ('a, 'b) t := ('a, 'b) Core.t

(** Incarnation of a [Functor2.Infix] with functional API of a [Functor2]. *)
module Infix
    (Core : Preface_specs.Functor2.CORE)
    (Operation : Preface_specs.Functor2.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) :
  Preface_specs.Functor2.INFIX with type ('a, 'b) t := ('a, 'b) Core.t
