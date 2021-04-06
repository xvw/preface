(** Modules for building {!Preface_specs.CATEGORY} modules.

    {1 Documentation}

    {2 Construction}

    Standard way to build a [Category]. *)

(** Incarnation of a [Category] using [id] and [compose]. *)
module Via_id_and_compose (Req : Preface_specs.Category.CORE) :
  Preface_specs.CATEGORY with type ('a, 'b) t = ('a, 'b) Req.t

(** Incarnation of a [Category] using a [monad] using the Kleisli composition. *)
module From_monad (Monad : Preface_specs.Monad.CORE) :
  Preface_specs.CATEGORY with type ('a, 'b) t = 'a -> 'b Monad.t

(** {2 Category composition}

    Some tools for composition between categories. *)

(** Product of two categories. *)
module Product (F : Preface_specs.CATEGORY) (G : Preface_specs.CATEGORY) :
  Preface_specs.CATEGORY with type ('a, 'b) t = ('a, 'b) F.t * ('a, 'b) G.t

(** {2 Manual construction}

    Advanced way to build a [Category], constructing and assembling a
    component-by-component a category. (In order to provide your own
    implementation for some features.) *)

(** Incarnation of a [Category] using each components of a [Category]. *)
module Via
    (Core : Preface_specs.Category.CORE)
    (Operation : Preface_specs.Category.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t)
    (Infix : Preface_specs.Category.INFIX
               with type ('a, 'b) t = ('a, 'b) Operation.t) :
  Preface_specs.CATEGORY with type ('a, 'b) t = ('a, 'b) Infix.t

(** Incarnation of [Category.Core] using [id] and [compose]. *)
module Core (Req : Preface_specs.Category.WITH_ID_AND_COMPOSE) :
  Preface_specs.Category.CORE with type ('a, 'b) t = ('a, 'b) Req.t

(** Incarnation of [Category.Operation] using [Core]. *)
module Operation (Core : Preface_specs.Category.CORE) :
  Preface_specs.Category.OPERATION with type ('a, 'b) t = ('a, 'b) Core.t

(** Incarnation of [Category.Infix] using [Core]. *)
module Infix (Core : Preface_specs.Category.CORE) :
  Preface_specs.Category.INFIX with type ('a, 'b) t = ('a, 'b) Core.t
