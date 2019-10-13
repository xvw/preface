(** Modules for building {!Preface_specs.MONAD} modules. *)

(** {1 Documentation} *)

(** {1 Construction} 

    Standard way to build a [Monad].
*)

module Make_via_bind (Core_via_bind : Preface_specs.Monad.CORE_VIA_BIND) :
  Preface_specs.MONAD with type 'a t = 'a Core_via_bind.t
(** Incarnation of a [Monad] with standard requirement ([return] and [bind]).
*)

module Make_via_map_and_join
    (Core_via_map_and_join : Preface_specs.Monad.CORE_VIA_MAP_AND_JOIN) :
  Preface_specs.MONAD with type 'a t = 'a Core_via_map_and_join.t
(** Incarnation of a [Monad] with map and join requirement 
    ([return], [join] and [map]).
*)

module Make_via_kleisli_composition
    (Core_via_kleisli_composition : Preface_specs.Monad
                                    .CORE_VIA_KLEISLI_COMPOSITION) :
  Preface_specs.MONAD with type 'a t = 'a Core_via_kleisli_composition.t
(** Incarnation of a [Monad] with kleisli composition requirement 
    ([return] and [compose_left_to_right]).
*)

(** {2 Manual construction} 
    
    Advanced way to build a [Monad], constructing and 
    assembling a component-by-component a monad. 
    (In order to provide your own implementation for some features.)
*)

module Make
    (Core : Preface_specs.Monad.CORE)
    (Operation : Preface_specs.Monad.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Monad.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Monad.SYNTAX with type 'a t = 'a Core.t) :
  Preface_specs.MONAD with type 'a t = 'a Core.t
(** Incarnation of a [Monad] using each components of a [Monad].*)

module Make_core_via_bind (Core : Preface_specs.Monad.CORE_VIA_BIND) :
  Preface_specs.Monad.CORE with type 'a t = 'a Core.t
(** Incarnation of a [Monad.Core] with standard requirement 
    ([return] and [bind]).
*)

module Make_core_via_map_and_join
    (Core : Preface_specs.Monad.CORE_VIA_MAP_AND_JOIN) :
  Preface_specs.Monad.CORE with type 'a t = 'a Core.t
(** Incarnation of a [Monad.Core] with map and join requirement 
    ([return], [join] and [map]).
*)

module Make_core_via_kleisli_composition
    (Core : Preface_specs.Monad.CORE_VIA_KLEISLI_COMPOSITION) :
  Preface_specs.Monad.CORE with type 'a t = 'a Core.t
(** Incarnation of a [Monad.Core] with kleisli composition requirement 
    ([return] and [compose_left_to_right]).
*)

module Make_operation (Core : Preface_specs.Monad.CORE) :
  Preface_specs.Monad.OPERATION with type 'a t = 'a Core.t
(** Incarnation of a [Monad.Operation] with [Monad.Core].*)

module Make_syntax (Core : Preface_specs.Monad.CORE) :
  Preface_specs.Monad.SYNTAX with type 'a t = 'a Core.t
(** Incarnation of a [Monad.Syntax] with [Monad.Core].*)

module Make_infix
    (Core : Preface_specs.Monad.CORE)
    (Operation : Preface_specs.Monad.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Monad.INFIX with type 'a t = 'a Core.t
(** Incarnation of a [Monad.Infix] with [Monad.Core] and [Monad.OPERATION].*)
