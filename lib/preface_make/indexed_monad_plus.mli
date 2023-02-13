(** Building a {!module:Preface_specs.Indexed_monad_plus} *)

(** {1 Using the minimal definition} *)

(** {2 Using return, bind, neutral and combine}

    Build a {!module-type:Preface_specs.INDEXED_MONAD_PLUS} using
    {!module-type:Preface_specs.Indexed_monad_plus.WITH_BIND}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_bind (Req : Preface_specs.Indexed_monad_plus.WITH_BIND) :
  Preface_specs.INDEXED_MONAD_PLUS with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Using return, map, join, neutral and combine}

    Build a {!module-type:Preface_specs.INDEXED_MONAD_PLUS} using
    {!module-type:Preface_specs.Indexed_onad_plus.WITH_MAP_AND_JOIN}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_map_and_join
    (Req : Preface_specs.Indexed_monad_plus.WITH_MAP_AND_JOIN) :
  Preface_specs.INDEXED_MONAD_PLUS with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Using return, neutral, combine and the kleisli composition}

    Build a {!module-type:Preface_specs.INDEXED_MONAD_PLUS} using
    {!module-type:Preface_specs.Indexed_monad_plus.WITH_KLEISLI_COMPOSITION}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_kleisli_composition
    (Req : Preface_specs.Indexed_monad_plus.WITH_KLEISLI_COMPOSITION) :
  Preface_specs.INDEXED_MONAD_PLUS with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Over a Monad and an Alternative}

    Build a {!module-type:Preface_specs.INDEXED_MONAD_PLUS} over a
    {!module-type:Preface_specs.INDEXED_MONAD} and an [neutral] and [combine]
    from an {!module-type:Preface_specs.INDEXED_ALTERNATIVE}. *)

module Over_monad_and_alternative
    (Monad : Preface_specs.INDEXED_MONAD)
    (Alternative : Preface_specs.INDEXED_ALTERNATIVE
                     with type ('a, 'index) t = ('a, 'index) Monad.t) :
  Preface_specs.INDEXED_MONAD_PLUS
    with type ('a, 'index) t = ('a, 'index) Alternative.t

(** {2 Over a Monad using neutral and combine}

    Build a {!module-type:Preface_specs.INDEXED_MONAD_PLUS} over a
    {!module-type:Preface_specs.INDEXED_MONAD} and using
    {!module-type:Preface_specs.Indexed_monad_plus.WITH_NEUTRAL_AND_COMBINE}. *)

module Over_monad
    (Monad : Preface_specs.INDEXED_MONAD)
    (Req : Preface_specs.Indexed_monad_plus.WITH_NEUTRAL_AND_COMBINE
             with type ('a, 'index) t = ('a, 'index) Monad.t) :
  Preface_specs.INDEXED_MONAD_PLUS with type ('a, 'index) t = ('a, 'index) Req.t

(** {1 Manual construction}

    Advanced way to build a {!module-type:Preface_specs.INDEXED_MONAD_PLUS},
    constructing and assembling a component-by-component of
    {!module-type:Preface_specs.INDEXED_MONAD_PLUS}. (In order to provide your
    own implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Indexed_monad_plus.CORE)
    (Operation : Preface_specs.Indexed_monad_plus.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t)
    (Infix : Preface_specs.Indexed_monad_plus.INFIX
               with type ('a, 'index) t = ('a, 'index) Core.t)
    (Syntax : Preface_specs.Indexed_monad_plus.SYNTAX
                with type ('a, 'index) t = ('a, 'index) Core.t) :
  Preface_specs.INDEXED_MONAD_PLUS
    with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Building Core} *)

module Core_via_bind (Req : Preface_specs.Indexed_monad_plus.WITH_BIND) :
  Preface_specs.Indexed_monad_plus.CORE
    with type ('a, 'index) t = ('a, 'index) Req.t

module Core_via_map_and_join
    (Req : Preface_specs.Indexed_monad_plus.WITH_MAP_AND_JOIN) :
  Preface_specs.Indexed_monad_plus.CORE
    with type ('a, 'index) t = ('a, 'index) Req.t

module Core_via_kleisli_composition
    (Req : Preface_specs.Indexed_monad_plus.WITH_KLEISLI_COMPOSITION) :
  Preface_specs.Indexed_monad_plus.CORE
    with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Indexed_monad_plus.CORE) :
  Preface_specs.Indexed_monad_plus.OPERATION
    with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Deriving Syntax} *)

module Syntax (Core : Preface_specs.Indexed_monad_plus.CORE) :
  Preface_specs.Indexed_monad_plus.SYNTAX
    with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Deriving Infix} *)

module Infix
    (Core : Preface_specs.Indexed_monad_plus.CORE)
    (Operation : Preface_specs.Indexed_monad_plus.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t) :
  Preface_specs.Indexed_monad_plus.INFIX
    with type ('a, 'index) t = ('a, 'index) Core.t
