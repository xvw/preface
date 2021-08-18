(** Building a {!module:Preface_specs.Free_monad} *)

(** {1 Using the minimal definition} *)

(** {2 Over a Functor}

    Build a {!module-type:Preface_specs.FREE_MONAD} over a
    {!module-type:Preface_specs.FUNCTOR}.*)

module Over_functor (F : Preface_specs.FUNCTOR) :
  Preface_specs.FREE_MONAD with type 'a f = 'a F.t

(** {2 Over an Applicative}

    Build a {!module-type:Preface_specs.FREE_MONAD} over a
    {!module-type:Preface_specs.APPLICATIVE}.*)

module Over_applicative (F : Preface_specs.APPLICATIVE) :
  Preface_specs.FREE_MONAD with type 'a f = 'a F.t

(** {2 Over a Selective}

    Build a {!module-type:Preface_specs.FREE_MONAD} over a
    {!module-type:Preface_specs.SELECTIVE}.*)

module Over_selective (F : Preface_specs.SELECTIVE) :
  Preface_specs.FREE_MONAD with type 'a f = 'a F.t

(** {2 Over a Monad}

    Build a {!module-type:Preface_specs.FREE_MONAD} over a
    {!module-type:Preface_specs.MONAD}.*)

module Over_monad (F : Preface_specs.MONAD) :
  Preface_specs.FREE_MONAD with type 'a f = 'a F.t
