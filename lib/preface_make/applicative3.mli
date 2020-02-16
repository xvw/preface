(** Modules for building {!Preface_specs.APPLICATIVE3} modules. *)

(** {1 Documentation} *)

(** {2 Construction}

    Standard way to build an [Applicative3 Functor]. *)

(** Incarnation of an [Applicative3] with standard requirements ([pure], [map]
    and [product]). *)
module Via_map_and_product
    (Core_with_map_and_product : Preface_specs.Applicative3
                                 .CORE_WITH_MAP_AND_PRODUCT) :
  Preface_specs.APPLICATIVE3
    with type ('a, 'b, 'c) t = ('a, 'b, 'c) Core_with_map_and_product.t

(** Incarnation of an [Applicative3] with standard requirements ([pure] and
    [apply]). *)
module Via_apply (Core_with_apply : Preface_specs.Applicative3.CORE_WITH_APPLY) :
  Preface_specs.APPLICATIVE3
    with type ('a, 'b, 'c) t = ('a, 'b, 'c) Core_with_apply.t

(** Incarnation of an [Applicative3] using a [Monad].*)

(* module Via_monad (Monad : Preface_specs.MONAD) :
 *   Preface_specs.APPLICATIVE with type ('a, 'b, 'c) t = ('a, 'b, 'c) Monad.t *)

(** {2 Manual construction}

    Advanced way to build an [Applicative3 Functor], constructing and assembling
    a component-by-component an applicative functor. (In order to provide your
    own implementation for some features.) *)

(** Incarnation of an [Applicative3] using each components of an [Applicative3]. *)
module Via
    (Core : Preface_specs.Applicative3.CORE)
    (Operation : Preface_specs.Applicative3.OPERATION
                   with type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t)
    (Infix : Preface_specs.Applicative3.INFIX
               with type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t)
    (Syntax : Preface_specs.Applicative3.SYNTAX
                with type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t) :
  Preface_specs.APPLICATIVE3 with type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t

(** Incarnation of an [Applicative3.Core] for an [('a, 'b, 'c)t] with standard
    Requirements ([pure], [map] and [product]). *)
module Core_via_map_and_product
    (Core : Preface_specs.Applicative3.CORE_WITH_MAP_AND_PRODUCT) :
  Preface_specs.Applicative3.CORE with type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t

(** Incarnation of an [Applicative3.Core] with standard requirements ([pure],
    [apply]). *)
module Core_via_apply (Core : Preface_specs.Applicative3.CORE_WITH_APPLY) :
  Preface_specs.Applicative3.CORE with type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t

(** Incarnation of an [Applicative3.Operation] with standard requirements
    ([pure], [map], [apply] and [product]). *)
module Operation (Core : Preface_specs.Applicative3.CORE) :
  Preface_specs.Applicative3.OPERATION
    with type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t

(** Incarnation of an [Applicative3.Syntax] with standard requirements ([pure],
    [map], [apply] and [product]). *)
module Syntax (Core : Preface_specs.Applicative3.CORE) :
  Preface_specs.Applicative3.SYNTAX
    with type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t

(** Incarnation of an [Applicative3.Infix] with standard requirements ([pure],
    [map], [apply] and [product]). *)
module Infix
    (Core : Preface_specs.Applicative3.CORE)
    (Operation : Preface_specs.Applicative3.OPERATION
                   with type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t) :
  Preface_specs.Applicative3.INFIX
    with type ('a, 'b, 'c) t = ('a, 'b, 'c) Core.t
