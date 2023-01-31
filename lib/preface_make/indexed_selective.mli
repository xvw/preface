(** Building a {!module:Preface_specs.Indexed_selective} *)

(** {1 Using the minimal definition} *)

(** {2 Over an Applicative using select}

    Build a {!module-type:Preface_specs.INDEXED_SELECTIVE} using
    {!module-type:Preface_specs.Indexed_selective.WITH_SELECT} on top of an
    {!module-type:Preface_specs.INDEXED_APPLICATIVE}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_applicative_via_select
    (Applicative : Preface_specs.INDEXED_APPLICATIVE)
    (Req : Preface_specs.Indexed_selective.WITH_SELECT
             with type ('a, 'index) t = ('a, 'index) Applicative.t) :
  Preface_specs.INDEXED_SELECTIVE with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Over an Applicative using branch}

    Build a {!module-type:Preface_specs.INDEXED_SELECTIVE} using
    {!module-type:Preface_specs.Indexed_selective.WITH_BRANCH} on top of an
    {!module-type:Preface_specs.INDEXED_APPLICATIVE}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_applicative_via_branch
    (Applicative : Preface_specs.INDEXED_APPLICATIVE)
    (Req : Preface_specs.Indexed_selective.WITH_BRANCH
             with type ('a, 'index) t = ('a, 'index) Applicative.t) :
  Preface_specs.INDEXED_SELECTIVE with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Over a Functor using select}

    Build a {!module-type:Preface_specs.INDEXED_SELECTIVE} using
    {!module-type:Preface_specs.Indexed_selective.WITH_SELECT} on top of an
    {!module-type:Preface_specs.INDEXED_FUNCTOR}.

    Standard method, using the minimal definition of an alt to derive its full
    API (including the Applicative API). *)

module Over_functor_via_select
    (Functor : Preface_specs.Indexed_functor.CORE)
    (Req : Preface_specs.Indexed_selective.WITH_PURE_AND_SELECT
             with type ('a, 'index) t = ('a, 'index) Functor.t) :
  Preface_specs.INDEXED_SELECTIVE with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Over a Functor using branch}

    Build a {!module-type:Preface_specs.INDEXED_SELECTIVE} using
    {!module-type:Preface_specs.Indexed_selective.WITH_BRANCH} on top of an
    {!module-type:Preface_specs.INDEXED_FUNCTOR}.

    Standard method, using the minimal definition of an alt to derive its full
    API (including the Applicative API). *)

module Over_functor_via_branch
    (Functor : Preface_specs.Indexed_functor.CORE)
    (Req : Preface_specs.Indexed_selective.WITH_PURE_AND_BRANCH
             with type ('a, 'index) t = ('a, 'index) Functor.t) :
  Preface_specs.INDEXED_SELECTIVE with type ('a, 'index) t = ('a, 'index) Req.t

(** {1 Manual construction}

    Advanced way to build a {!module-type:Preface_specs.INDEXED_SELECTIVE},
    constructing and assembling a component-by-component of
    {!module-type:Preface_specs.INDEXED_SELECTIVE}. (In order to provide your
    own implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Indexed_selective.CORE)
    (Operation : Preface_specs.Indexed_selective.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t)
    (Infix : Preface_specs.Indexed_selective.INFIX
               with type ('a, 'index) t = ('a, 'index) Core.t)
    (Syntax : Preface_specs.Indexed_selective.SYNTAX
                with type ('a, 'index) t = ('a, 'index) Core.t) :
  Preface_specs.INDEXED_SELECTIVE with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Building Core} *)

module Core_over_functor_via_select
    (Functor : Preface_specs.Indexed_functor.CORE)
    (Req : Preface_specs.Indexed_selective.WITH_PURE_AND_SELECT
             with type ('a, 'index) t = ('a, 'index) Functor.t) :
  Preface_specs.Indexed_selective.CORE
    with type ('a, 'index) t = ('a, 'index) Req.t

module Core_over_functor_via_branch
    (Functor : Preface_specs.Indexed_functor.CORE)
    (Req : Preface_specs.Indexed_selective.WITH_PURE_AND_BRANCH
             with type ('a, 'index) t = ('a, 'index) Functor.t) :
  Preface_specs.Indexed_selective.CORE
    with type ('a, 'index) t = ('a, 'index) Req.t

module Core_over_applicative_via_select
    (Applicative : Preface_specs.Indexed_applicative.CORE)
    (Req : Preface_specs.Indexed_selective.WITH_SELECT
             with type ('a, 'index) t = ('a, 'index) Applicative.t) :
  Preface_specs.Indexed_selective.CORE
    with type ('a, 'index) t = ('a, 'index) Req.t

module Core_over_applicative_via_branch
    (Applicative : Preface_specs.Indexed_applicative.CORE)
    (Req : Preface_specs.Indexed_selective.WITH_BRANCH
             with type ('a, 'index) t = ('a, 'index) Applicative.t) :
  Preface_specs.Indexed_selective.CORE
    with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Indexed_selective.CORE) :
  Preface_specs.Indexed_selective.OPERATION
    with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Deriving Infix} *)

module Infix
    (Core : Preface_specs.Indexed_selective.CORE)
    (Operation : Preface_specs.Indexed_selective.OPERATION
                   with type ('a, 'index) t = ('a, 'index) Core.t) :
  Preface_specs.Indexed_selective.INFIX
    with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Deriving Syntax} *)

module Syntax (Core : Preface_specs.Indexed_selective.CORE) :
  Preface_specs.Indexed_selective.SYNTAX
    with type ('a, 'index) t = ('a, 'index) Core.t

(** {2 Deriving Select from a Monad} *)

module Select_from_monad (Monad : Preface_specs.Indexed_monad.CORE) :
  Preface_specs.Indexed_selective.WITH_SELECT
    with type ('a, 'index) t = ('a, 'index) Monad.t
