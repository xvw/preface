(** Modules for building {!Preface_specs.FOLDABLE} modules. *)

(** {1 Tutorial}

    A [Foldable] can be (easily) built defining [fold_map'] or [fold_right].
    There is a small nuance between [fold_map'] and [fold_map] in order to deal
    with first-class modules polymorphism. So the two first arguments of
    [fold_map'] are [neutral] and [combine] from [Monoid].*)

(** {1 Documentation} *)

(** {2 Construction}

    Standard way to build a [Foldable]. *)

(** Incarnation of a [Foldable] with [fold_map'] as requirement. *)
module Via_fold_map (F : Preface_specs.Foldable.CORE_WITH_FOLD_MAP) :
  Preface_specs.FOLDABLE with type 'a t = 'a F.t

(** Incarnation of a [Foldable] with [fold_right] as requirement. *)
module Via_fold_right (F : Preface_specs.Foldable.CORE_WITH_FOLD_RIGHT) :
  Preface_specs.FOLDABLE with type 'a t = 'a F.t

(** {2 Foldable composition}

    Some tools for composition between Foldables. *)

(** Right-to-left composition of Foldables.*)
module Composition (F : Preface_specs.FOLDABLE) (G : Preface_specs.FOLDABLE) :
  Preface_specs.FOLDABLE with type 'a t = 'a G.t F.t

(** Right-to-left composition of Foldables.*)

(** {2 Manual construction}

    Advanced way to build a [Foldable], constructing and assembling a
    component-by-component a foldable. (In order to provide your own
    implementation for some features.) *)

(** Incarnation of a [Foldable] using each components of a [Foldable]. *)
module Via
    (C : Preface_specs.Foldable.CORE)
    (O : Preface_specs.Foldable.OPERATION with type 'a t = 'a C.t) :
  Preface_specs.FOLDABLE with type 'a t = 'a O.t

(** Incarnation of a [Foldable.CORE] with [fold_right] as requirement. *)
module Core_via_fold_right (F : Preface_specs.Foldable.CORE_WITH_FOLD_RIGHT) :
  Preface_specs.Foldable.CORE with type 'a t = 'a F.t

(** Incarnation of a [Foldable.CORE] with [fold_map'] as requirement. *)
module Core_via_fold_map (F : Preface_specs.Foldable.CORE_WITH_FOLD_MAP) :
  Preface_specs.Foldable.CORE with type 'a t = 'a F.t

(** Incarnation of a [Foldable.OPERATION] with [Foldable.CORE]. *)
module Operation (C : Preface_specs.Foldable.CORE) :
  Preface_specs.Foldable.OPERATION with type 'a t = 'a C.t
