(** Modules for building {!Preface_specs.MONAD_PLUS} modules.

    {1 Documentation} *)

(** {2 Construction}

    Standard way to build a [Monad_plus]. *)

(** Incarnation of a [Monad_plus] with standard requirement ([return], [bind],
    [neutral] and [combine]). *)
module Via_bind (Core_with_bind : Preface_specs.Monad_plus.CORE_WITH_BIND) :
  Preface_specs.MONAD_PLUS with type 'a t = 'a Core_with_bind.t

(** Incarnation of a [Monad_plus] with standard requirement ([return], [map],
    [join], [neutral] and [combine]). *)
module Via_map_and_join
    (Core_with_map_and_join : Preface_specs.Monad_plus.CORE_WITH_MAP_AND_JOIN) :
  Preface_specs.MONAD_PLUS with type 'a t = 'a Core_with_map_and_join.t

(** Incarnation of a [Monad_plus] with standard requirement ([return],
    [compose_left_to_right], [neutral] and [combine]). *)
module Via_kleisli_composition
    (Core_with_kleisli_composition : Preface_specs.Monad_plus
                                     .CORE_WITH_KLEISLI_COMPOSITION) :
  Preface_specs.MONAD_PLUS with type 'a t = 'a Core_with_kleisli_composition.t

(** Incarnation of a [Monad_plus] over a [Monad] and an [Alternative].*)
module Over_monad_and_alternative
    (Monad : Preface_specs.MONAD)
    (Alternative : Preface_specs.ALTERNATIVE with type 'a t = 'a Monad.t) :
  Preface_specs.MONAD_PLUS with type 'a t = 'a Alternative.t

(** Incarnation of a [Monad_plus] over a [Monad].*)
module Over_monad
    (Monad : Preface_specs.MONAD)
    (Core : Preface_specs.Monad_plus.CORE_WITH_NEUTRAL_AND_COMBINE
              with type 'a t = 'a Monad.t) :
  Preface_specs.MONAD_PLUS with type 'a t = 'a Core.t

(** Incarnation of a [Monad_plus] using an [Arrow_apply] (for Monad) and
    [Arrow_plus] (for Alternative) via [Arrow Monad] encoding.*)
module From_arrow_apply_and_arrow_plus
    (A : Preface_specs.ARROW_APPLY)
    (P : Preface_specs.ARROW_PLUS with type ('a, 'b) t = ('a, 'b) A.t) :
  Preface_specs.MONAD_PLUS with type 'a t = (unit, 'a) P.t

(** {2 Manual construction}

    Advanced way to build a [Monad_plus], constructing and assembling a
    component-by-component a monad plus. (In order to provide your own
    implementation for some features.) *)

(** Incarnation of a [Monad_plus] using each components of a [Monad_plus]. *)
module Via
    (Core : Preface_specs.Monad_plus.CORE)
    (Operation : Preface_specs.Monad_plus.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Monad_plus.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Monad_plus.SYNTAX with type 'a t = 'a Core.t) :
  Preface_specs.MONAD_PLUS with type 'a t = 'a Core.t

(** Incarnation of a [Monad_plus.Core] with standard requirement ([return],
    [bind], [neutral] and [combine]). *)
module Core_via_bind (Core : Preface_specs.Monad_plus.CORE_WITH_BIND) :
  Preface_specs.Monad_plus.CORE with type 'a t = 'a Core.t

(** Incarnation of a [Monad_plus.Core] with standard requirement ([return],
    [map], [join], [neutral] and [combine]). *)
module Core_via_map_and_join
    (Core : Preface_specs.Monad_plus.CORE_WITH_MAP_AND_JOIN) :
  Preface_specs.Monad_plus.CORE with type 'a t = 'a Core.t

(** Incarnation of a [Monad_plus.Core] with standard requirement ([return],
    [compose_left_to_right], [neutral] and [combine]). *)
module Core_via_kleisli_composition
    (Core : Preface_specs.Monad_plus.CORE_WITH_KLEISLI_COMPOSITION) :
  Preface_specs.Monad_plus.CORE with type 'a t = 'a Core.t

(** Incarnation of a [Monad_plus.Operation] with [Monad_plus.Core].*)
module Operation (Core : Preface_specs.Monad_plus.CORE) :
  Preface_specs.Monad_plus.OPERATION with type 'a t = 'a Core.t

(** Incarnation of a [Monad_plus.Syntax] with [Monad_plus.Core].*)
module Syntax (Core : Preface_specs.Monad_plus.CORE) :
  Preface_specs.Monad_plus.SYNTAX with type 'a t = 'a Core.t

(** Incarnation of a [Monad_plus.Infix] with [Monad_plus.Core] and
    [Monad_plus.OPERATION].*)
module Infix
    (Core : Preface_specs.Monad_plus.CORE)
    (Operation : Preface_specs.Monad_plus.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Monad_plus.INFIX with type 'a t = 'a Core.t
