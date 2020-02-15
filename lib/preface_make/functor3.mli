(** Modules for building {!Preface_specs.FUNCTOR3} modules. *)

(** {2 Construction}

    Standard way to build a [Functor3]. *)

(** Incarnation of a [Functor3] with standard requirements ([map]).*)
module Via_map (Core : Preface_specs.Functor3.CORE) :
  Preface_specs.FUNCTOR3 with type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t

(** {2 Manual construction}

    Advanced way to build a [Functor3], constructing and assembling a
    component-by-component functor. (In order to provide your own implementation
    for some features.) *)

(** Incarnation of a [Functor3] using each components of a [Functor3]. *)
module Via
    (Core : Preface_specs.Functor3.CORE)
    (Operation : Preface_specs.Functor3.OPERATION
                   with type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t)
    (Infix : Preface_specs.Functor3.INFIX
               with type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t) :
  Preface_specs.FUNCTOR3 with type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t

(** Incarnation of a [Functor3.Operation] with standard Requirements ([map]). *)
module Operation (Core : Preface_specs.Functor3.CORE) :
  Preface_specs.Functor3.OPERATION
    with type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t

(** Incarnation of a [Functor3.Infix] with functional API of a [Functor3]. *)
module Infix
    (Core : Preface_specs.Functor3.CORE)
    (Operation : Preface_specs.Functor3.OPERATION
                   with type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t) :
  Preface_specs.Functor3.INFIX with type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t
