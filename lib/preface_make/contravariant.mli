(** Modules for building {!Preface_specs.CONTRAVARIANT} modules.

    {1 Documentation}

    {2 Construction}

    Standard way to build a [Contravariant Functor]. *)

(** Incarnation of a [Contravariant] using [contramap]. *)
module Via_contramap (Core : Preface_specs.Contravariant.CORE) :
  Preface_specs.CONTRAVARIANT with type 'a t = 'a Core.t

(** {2 Contravariant composition}

    Some tools for composition between Contravariant functors. *)

(** Left-to-right composition of contravariant with functor.*)
module Composition (F : Preface_specs.FUNCTOR) (G : Preface_specs.CONTRAVARIANT) :
  Preface_specs.CONTRAVARIANT with type 'a t = 'a G.t F.t

(** {2 Manual construction}

    Advanced way to build a [Contravariant], constructing and assembling a
    component-by-component an arrow. (In order to provide your own
    implementation for some features.) *)

(** Incarnation of an [Contravariant] using each components of a
    [Contravariant]. *)
module Via
    (Core : Preface_specs.Contravariant.CORE)
    (Operation : Preface_specs.Contravariant.OPERATION
                   with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Contravariant.INFIX with type 'a t = 'a Operation.t) :
  Preface_specs.CONTRAVARIANT with type 'a t = 'a Infix.t

(** Incarnation of [Contravariant.Operation] using [Core]. *)
module Operation (Core : Preface_specs.Contravariant.CORE) :
  Preface_specs.Contravariant.OPERATION with type 'a t = 'a Core.t

(** Incarnation of [Contravariant.Infix] using [Core] and [Operation]. *)
module Infix
    (Core : Preface_specs.Contravariant.CORE)
    (Operation : Preface_specs.Contravariant.OPERATION
                   with type 'a t = 'a Core.t) :
  Preface_specs.Contravariant.INFIX with type 'a t = 'a Operation.t
