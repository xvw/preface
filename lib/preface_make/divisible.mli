(** Building a {!module:Preface_specs.Divisible} *)

(** {1 Using the minimal definition} *)

(** {2 Using divide and conquer}

    Build a {!module-type:Preface_specs.DIVISBLE} using
    {!module-type:Preface_specs.Divisible.WITH_DIVIDE_AND_CONQUER}. [contramap]
    will be derived using [divide] and [conquer].

    Standard method, using the minimal definition of a Divisible to derive its
    full API. *)

module Via_divide_and_conquer
    (Req : Preface_specs.Divisible.WITH_DIVIDE_AND_CONQUER) :
  Preface_specs.DIVISIBLE with type 'a t = 'a Req.t

(** {2 Using divide, conquer and contramap}

    Build a {!module-type:Preface_specs.DIVISBLE} using
    {!module-type:Preface_specs.Divisible.WITH_CONTRAMAP_AND_DIVIDE_AND_CONQUER}.

    Standard method, using the minimal definition of a Divisible to derive its
    full API. *)

module Via_contramap_and_divide_and_conquer
    (Req : Preface_specs.Divisible.WITH_CONTRAMAP_AND_DIVIDE_AND_CONQUER) :
  Preface_specs.DIVISIBLE with type 'a t = 'a Req.t

(** {1 Over Contravariant functor}

    Produces a {!module-type:Preface_specs.DIVISBLE} from a
    {!module-type:Preface_specs.CONTRAVARIANT}. *)

module Over_contravariant
    (Contravariant : Preface_specs.Contravariant.CORE)
    (Req : Preface_specs.Divisible.WITH_DIVIDE_AND_CONQUER
             with type 'a t = 'a Contravariant.t) :
  Preface_specs.DIVISIBLE with type 'a t = 'a Req.t

(** {1 Manual construction}

    Advanced way to build an {!module-type:Preface_specs.DIVISBLE}, constructing
    and assembling a component-by-component of
    {!module-type:Preface_specs.DIVISBLE}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Divisible.CORE)
    (Operation : Preface_specs.Divisible.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Divisible.INFIX with type 'a t = 'a Operation.t) :
  Preface_specs.DIVISIBLE with type 'a t = 'a Infix.t

(** {2 Building Core} *)

module Core_via_divide_and_conquer
    (Req : Preface_specs.Divisible.WITH_DIVIDE_AND_CONQUER) :
  Preface_specs.Divisible.CORE with type 'a t = 'a Req.t

module Core_via_contramap_and_divide_and_conquer
    (Req : Preface_specs.Divisible.WITH_CONTRAMAP_AND_DIVIDE_AND_CONQUER) :
  Preface_specs.Divisible.CORE with type 'a t = 'a Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Divisible.CORE) :
  Preface_specs.Divisible.OPERATION with type 'a t = 'a Core.t

(** {2 Deriving Infix} *)

module Infix
    (Core : Preface_specs.Divisible.CORE)
    (Operation : Preface_specs.Divisible.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Divisible.INFIX with type 'a t = 'a Operation.t
