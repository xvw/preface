(** Modules for building {!Preface_specs.APPLICATIVE2} modules. *)

(** {1 Documentation} *)

(** {2 Construction}

    Standard way to build an [Applicative2 Functor]. *)

(** Incarnation of an [Applicative2] with standard requirements ([pure], [map]
    and [product]). *)
module Via_map_and_product
    (Core_with_map_and_product : Preface_specs.Applicative2
                                 .CORE_WITH_MAP_AND_PRODUCT) :
  Preface_specs.APPLICATIVE2
    with type ('a, 'b) t := ('a, 'b) Core_with_map_and_product.t

(** Incarnation of an [Applicative2] with standard requirements ([pure] and
    [apply]). *)
module Via_apply (Core_with_apply : Preface_specs.Applicative2.CORE_WITH_APPLY) :
  Preface_specs.APPLICATIVE2 with type ('a, 'b) t := ('a, 'b) Core_with_apply.t

(** Incarnation of an [Applicative2] using a [Monad].*)

(* module Via_monad (Monad : Preface_specs.MONAD) :
 *   Preface_specs.APPLICATIVE with type ('a, 'b) t = ('a, 'b) Monad.t *)

(** {2 Manual construction}

    Advanced way to build an [Applicative2 Functor], constructing and assembling
    a component-by-component an applicative functor. (In order to provide your
    own implementation for some features.) *)

(** Incarnation of an [Applicative2] using each components of an [Applicative2]. *)
module Via
    (Core : Preface_specs.Applicative2.CORE)
    (Operation : Preface_specs.Applicative2.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t)
    (Infix : Preface_specs.Applicative2.INFIX
               with type ('a, 'b) t = ('a, 'b) Core.t)
    (Syntax : Preface_specs.Applicative2.SYNTAX
                with type ('a, 'b) t = ('a, 'b) Core.t) :
  Preface_specs.APPLICATIVE2 with type ('a, 'b) t := ('a, 'b) Core.t

(** Incarnation of an [Applicative2.Core] for an [('a, 'b)t] with standard
    Requirements ([pure], [map] and [product]). *)
module Core_via_map_and_product
    (Core : Preface_specs.Applicative2.CORE_WITH_MAP_AND_PRODUCT) :
  Preface_specs.Applicative2.CORE with type ('a, 'b) t := ('a, 'b) Core.t

(** Incarnation of an [Applicative2.Core] with standard requirements ([pure],
    [apply]). *)
module Core_via_apply (Core : Preface_specs.Applicative2.CORE_WITH_APPLY) :
  Preface_specs.Applicative2.CORE with type ('a, 'b) t := ('a, 'b) Core.t

(** Incarnation of an [Applicative2.Operation] with standard requirements
    ([pure], [map], [apply] and [product]). *)
module Operation (Core : Preface_specs.Applicative2.CORE) :
  Preface_specs.Applicative2.OPERATION with type ('a, 'b) t := ('a, 'b) Core.t

(** Incarnation of an [Applicative2.Syntax] with standard requirements ([pure],
    [map], [apply] and [product]). *)
module Syntax (Core : Preface_specs.Applicative2.CORE) :
  Preface_specs.Applicative2.SYNTAX with type ('a, 'b) t := ('a, 'b) Core.t

(** Incarnation of an [Applicative2.Infix] with standard requirements ([pure],
    [map], [apply] and [product]). *)
module Infix
    (Core : Preface_specs.Applicative2.CORE)
    (Operation : Preface_specs.Applicative2.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) :
  Preface_specs.Applicative2.INFIX with type ('a, 'b) t := ('a, 'b) Core.t
