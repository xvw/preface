(** Building a {!module:Preface_specs.Traversable} *)

(** {1 Using the minimal definition} *)

(** {2 Over an Applicative using traverse}

    Build a {!module-type:Preface_specs.TRAVERSABLE} using
    {!module-type:Preface_specs.Traversable.WITH_TRAVERSE} over an
    {!module-type:Preface_specs.APPLICATIVE}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_applicative
    (A : Preface_specs.APPLICATIVE)
    (Req : Preface_specs.Traversable.WITH_TRAVERSE with type 'a t = 'a A.t) :
  Preface_specs.TRAVERSABLE
    with type 'a t = 'a Req.t
     and type 'a iter = 'a Req.iter

(** {2 Over a Monad using traverse}

    Build a {!module-type:Preface_specs.TRAVERSABLE} using
    {!module-type:Preface_specs.Traversable.WITH_TRAVERSE} over a
    {!module-type:Preface_specs.MONAD}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Over_monad
    (M : Preface_specs.MONAD)
    (Req : Preface_specs.Traversable.WITH_TRAVERSE with type 'a t = 'a M.t) :
  Preface_specs.TRAVERSABLE
    with type 'a t = 'a Req.t
     and type 'a iter = 'a Req.iter

(** {1 Merging API}

    Since {!module-type:Preface_specs.TRAVERSABLE} can be define for traversing
    iterable structure being a {!module-type:Preface_specs.MONAD} or an
    {!module-type:Preface_specs.APPLICATIVE}, there is some module (like [List]
    or [Nonempty_list] which define {!module-type:Preface_specs.APPLICATIVE} or
    {!module-type:Preface_specs.MONAD} modules with in a
    {!module-type:Preface_specs.TRAVERSABLE} module. *)

(** {2 Merge Traversable with Monad}

    Produces a {!module-type:Preface_specs.MONAD} module with a
    {!module-type:Preface_specs.TRAVERSABLE} module (which relies on monadic
    traversal). *)

module Join_with_monad
    (I : Preface_specs.MONAD) (T : functor (M : Preface_specs.MONAD) ->
      Preface_specs.TRAVERSABLE
        with type 'a t = 'a M.t
         and type 'a iter = 'a I.t) :
  Preface_specs.Traversable.API_OVER_MONAD with type 'a t = 'a I.t

(** {2 Merge Traversable with Applicative}

    Produces a {!module-type:Preface_specs.APPLICATIVE} module with a
    {!module-type:Preface_specs.TRAVERSABLE} module (which relies on applicative
    traversal). *)

module Join_with_applicative
    (I : Preface_specs.APPLICATIVE)
    (T : functor
      (A : Preface_specs.APPLICATIVE)
      ->
      Preface_specs.TRAVERSABLE
        with type 'a t = 'a A.t
         and type 'a iter = 'a I.t) :
  Preface_specs.Traversable.API_OVER_APPLICATIVE with type 'a t = 'a I.t

(** {1 Manual construction}

    Advanced way to build a {!module-type:Preface_specs.TRAVERSABLE},
    constructing and assembling a component-by-component of
    {!module-type:Preface_specs.TRAVERSABLE}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (C : Preface_specs.Traversable.CORE)
    (O : Preface_specs.Traversable.OPERATION
           with type 'a t = 'a C.t
            and type 'a iter = 'a C.iter) :
  Preface_specs.TRAVERSABLE with type 'a t = 'a C.t and type 'a iter = 'a C.iter

(** {2 Building Core} *)

module Core (Req : Preface_specs.Traversable.WITH_TRAVERSE) :
  Preface_specs.Traversable.CORE
    with type 'a t = 'a Req.t
     and type 'a iter = 'a Req.iter

module Core_over_applicative
    (A : Preface_specs.APPLICATIVE)
    (Req : Preface_specs.Traversable.WITH_TRAVERSE with type 'a t = 'a A.t) :
  Preface_specs.Traversable.CORE
    with type 'a t = 'a Req.t
     and type 'a iter = 'a Req.iter

module Core_over_monad
    (M : Preface_specs.MONAD)
    (Req : Preface_specs.Traversable.WITH_TRAVERSE with type 'a t = 'a M.t) :
  Preface_specs.Traversable.CORE
    with type 'a t = 'a Req.t
     and type 'a iter = 'a Req.iter

(** {2 Deriving Operation} *)

module Operation (C : Preface_specs.Traversable.CORE) :
  Preface_specs.Traversable.OPERATION
    with type 'a t = 'a C.t
     and type 'a iter = 'a C.iter
