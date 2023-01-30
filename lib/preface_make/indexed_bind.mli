(** Building a {!module:Preface_specs.Indexed_bind} *)

(** {1 Using the minimal definition} *)

(** {2 Using map and bind}

    Build a {!module-type:Preface_specs.Indexed_bind} using
    {!module-type:Preface_specs.Indexed_Bind.WITH_RETURN_AND_BIND}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_map_and_bind (Req : Preface_specs.Indexed_bind.WITH_MAP_AND_BIND) :
  Preface_specs.INDEXED_BIND with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Using bind over functor}

    Build a {!module-type:Preface_specs.INDEXED_BIND} using
    {!module-type:Preface_specs.Indexed_functor.WITH_MAP} and
    {!module-type:Preface_specs.Indexed_bind.WITH_BIND}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_functor_via_bind
    (Functor : Preface_specs.Indexed_functor.WITH_MAP)
    (Req : Preface_specs.Indexed_bind.WITH_BIND
             with type ('a, 'index) t = ('a, 'index) Functor.t) :
  Preface_specs.INDEXED_BIND with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Using return, map and join}

    Build a {!module-type:Preface_specs.INDEXED_BIND} using
    {!module-type:Preface_specs.Indexed_bind.WITH_RETURN_MAP_AND_JOIN}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_map_and_join (Req : Preface_specs.Indexed_bind.WITH_MAP_AND_JOIN) :
  Preface_specs.INDEXED_BIND with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Using return and the kleisli composition}

    Build a {!module-type:Preface_specs.INDEXED_BIND} using rap contenders
    {!module-type:Preface_specs.Indexed_bind.WITH_RETURN_AND_KLEISLI_COMPOSITION}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_map_and_kleisli_composition
    (Req : Preface_specs.Indexed_bind.WITH_MAP_AND_KLEISLI_COMPOSITION) :
  Preface_specs.INDEXED_BIND with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Using kleisli composition over functor}

    Build a {!module-type:Preface_specs.INDEXED_BIND} using
    {!module-type:Preface_specs.Indexed_functor.WITH_MAP} and
    {!module-type:Preface_specs.Indexed_bind.WITH_KLEISLI_COMPOSITION}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_functor_via_kleisli_composition
    (Functor : Preface_specs.Indexed_functor.WITH_MAP)
    (Req : Preface_specs.Indexed_bind.WITH_KLEISLI_COMPOSITION
             with type ('a, 'index) t = ('a, 'index) Functor.t) :
  Preface_specs.INDEXED_BIND with type ('a, 'index) t = ('a, 'index) Req.t

(** {1 Manual construction}

    Advanced way to build a {!module-type:Preface_specs.INDEXED_BIND},
    constructing and assembling a component-by-component of
    {!module-type:Preface_specs.INDEXED_BIND}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Indexed_bind.CORE)
    (Operation : Preface_specs.Indexed_bind.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t)
    (Infix : Preface_specs.Indexed_bind.INFIX
               with type ('a, 'index) t = ('a, 'index) Core.t)
    (Syntax : Preface_specs.Indexed_bind.SYNTAX
                with type ('a, 'index) t = ('a, 'index) Core.t) :
  Preface_specs.INDEXED_BIND with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Building Core} *)

module Core_via_map_and_bind
    (Req : Preface_specs.Indexed_bind.WITH_MAP_AND_BIND) :
  Preface_specs.Indexed_bind.CORE with type ('a, 'index) t = ('a, 'index) Req.t

module Core_via_map_and_join
    (Req : Preface_specs.Indexed_bind.WITH_MAP_AND_JOIN) :
  Preface_specs.Indexed_bind.CORE with type ('a, 'index) t = ('a, 'index) Req.t

module Core_via_map_and_kleisli_composition
    (Req : Preface_specs.Indexed_bind.WITH_MAP_AND_KLEISLI_COMPOSITION) :
  Preface_specs.Indexed_bind.CORE with type ('a, 'index) t = ('a, 'index) Req.t

module Core_over_functor_via_bind
    (Functor : Preface_specs.Indexed_functor.WITH_MAP)
    (Req : Preface_specs.Indexed_bind.WITH_BIND
             with type ('a, 'index) t = ('a, 'index) Functor.t) :
  Preface_specs.Indexed_bind.CORE with type ('a, 'index) t = ('a, 'index) Req.t

module Core_over_functor_via_kleisli_composition
    (Functor : Preface_specs.Indexed_functor.WITH_MAP)
    (Req : Preface_specs.Indexed_bind.WITH_KLEISLI_COMPOSITION
             with type ('a, 'index) t = ('a, 'index) Functor.t) :
  Preface_specs.Indexed_bind.CORE with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Indexed_bind.CORE) :
  Preface_specs.Indexed_bind.OPERATION
    with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Deriving Syntax} *)

module Syntax (Core : Preface_specs.Indexed_bind.CORE) :
  Preface_specs.Indexed_bind.SYNTAX
    with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Deriving Infix} *)

module Infix
    (Core : Preface_specs.Indexed_bind.CORE)
    (Operation : Preface_specs.Indexed_bind.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t) :
  Preface_specs.Indexed_bind.INFIX
    with type ('a, 'index) t = ('a, 'index) Core.t
