(** Building a {!module:Preface_specs.Indexed_monad} *)

(** {1 Using the minimal definition} *)

(** {2 Using return and bind}

    Build a {!module-type:Preface_specs.INDEXED_MONAD} using
    {!module-type:Preface_specs.Indexed_monad.WITH_RETURN_AND_BIND}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_return_and_bind
    (Req : Preface_specs.Indexed_monad.WITH_RETURN_AND_BIND) :
  Preface_specs.INDEXED_MONAD with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Using return, map and join}

    Build a {!module-type:Preface_specs.INDEXED_MONAD} using
    {!module-type:Preface_specs.Indexed_monad.WITH_RETURN_MAP_AND_JOIN}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_return_map_and_join
    (Req : Preface_specs.Indexed_monad.WITH_RETURN_MAP_AND_JOIN) :
  Preface_specs.INDEXED_MONAD with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Using return and the kleisli composition}

    Build a {!module-type:Preface_specs.INDEXED_MONAD} using
    {!module-type:Preface_specs.Indexed_monad.WITH_RETURN_AND_KLEISLI_COMPOSITION}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_return_and_kleisli_composition
    (Req : Preface_specs.Indexed_monad.WITH_RETURN_AND_KLEISLI_COMPOSITION) :
  Preface_specs.INDEXED_MONAD with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Indexed_monad.CORE)
    (Operation : Preface_specs.Indexed_monad.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t)
    (Infix : Preface_specs.Indexed_monad.INFIX
               with type ('a, 'index) t = ('a, 'index) Core.t)
    (Syntax : Preface_specs.Indexed_monad.SYNTAX
                with type ('a, 'index) t = ('a, 'index) Core.t) :
  Preface_specs.INDEXED_MONAD with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Building Core} *)

module Core_via_return_and_bind
    (Req : Preface_specs.Indexed_monad.WITH_RETURN_AND_BIND) :
  Preface_specs.Indexed_monad.CORE with type ('a, 'index) t = ('a, 'index) Req.t

module Core_via_return_map_and_join
    (Req : Preface_specs.Indexed_monad.WITH_RETURN_MAP_AND_JOIN) :
  Preface_specs.Indexed_monad.CORE with type ('a, 'index) t = ('a, 'index) Req.t

module Core_via_return_and_kleisli_composition
    (Req : Preface_specs.Indexed_monad.WITH_RETURN_AND_KLEISLI_COMPOSITION) :
  Preface_specs.Indexed_monad.CORE with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Indexed_monad.CORE) :
  Preface_specs.Indexed_monad.OPERATION
    with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Deriving Syntax} *)

module Syntax (Core : Preface_specs.Indexed_monad.CORE) :
  Preface_specs.Indexed_monad.SYNTAX
    with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Deriving Infix} *)

module Infix
    (Core : Preface_specs.Indexed_monad.CORE)
    (Operation : Preface_specs.Indexed_monad.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t) :
  Preface_specs.Indexed_monad.INFIX
    with type ('a, 'index) t = ('a, 'index) Core.t
