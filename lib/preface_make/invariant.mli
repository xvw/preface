(** Building a {!module:Preface_specs.Invariant} *)

(** {1 Using the minimal definition}

    Build a {!module-type:Preface_specs.INVARIANT} using
    {!module-type:Preface_specs.Invariant.WITH_INVMAP}.

    Standard method, using the minimal definition of a contravariant functor to
    derive its full API. *)

module Via_invmap (Req : Preface_specs.Invariant.WITH_INVMAP) :
  Preface_specs.Invariant.API with type 'a t = 'a Req.t

(** {1 From other abstraction} *)

(** {2 From a Functor}

    Specialize a {!module-type:Preface_specs.FUNCTOR} into a
    {!module-type:Preface_specs.INVARIANT}. *)

module From_functor (F : Preface_specs.Functor.CORE) :
  Preface_specs.Invariant.API with type 'a t = 'a F.t

(** {2 From a Contravariant}

    Specialize a {!module-type:Preface_specs.CONTRAVARIANT} into a
    {!module-type:Preface_specs.INVARIANT}. *)

module From_contravariant (F : Preface_specs.Contravariant.CORE) :
  Preface_specs.Invariant.API with type 'a t = 'a F.t
