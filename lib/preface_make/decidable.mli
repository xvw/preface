(** Building a {!module:Preface_specs.Decidable} *)

(** {1 Using the minimal definition} *)

(** {2 Using divide, conquer, choose and lose}

    Build a {!module-type:Preface_specs.DECIDABLE} using
    {!module-type:Preface_specs.Divisible.WITH_DIVIDE_AND_CONQUER} and
    {!module-type:Preface_specs.Decidable.WITH_LOSE_AND_CHOOSE}. [contramap]
    will be derived using [divide] and [conquer].

    Standard method, using the minimal definition of a Divisible to derive its
    full API. *)

module Via_divide_and_conquer
    (Divisible_req : Preface_specs.Divisible.WITH_DIVIDE_AND_CONQUER)
    (Req : Preface_specs.Decidable.WITH_LOSE_AND_CHOOSE
             with type 'a t = 'a Divisible_req.t) :
  Preface_specs.DECIDABLE with type 'a t = 'a Req.t

(** {2 Using divide, conquer, choose, lose and contramap}

    Build a {!module-type:Preface_specs.DECIDABLE} using
    {!module-type:Preface_specs.Decidable.WITH_CONTRAMAP_AND_DIVIDE_AND_CONQUER}
    and {!module-type:Preface_specs.Decidable.WITH_LOSE_AND_CHOOSE}.

    Standard method, using the minimal definition of a Divisible to derive its
    full API. *)

module Via_contramap_and_divide_and_conquer
    (Divisible_req : Preface_specs.Divisible
                     .WITH_CONTRAMAP_AND_DIVIDE_AND_CONQUER)
    (Req : Preface_specs.Decidable.WITH_LOSE_AND_CHOOSE
             with type 'a t = 'a Divisible_req.t) :
  Preface_specs.DECIDABLE with type 'a t = 'a Req.t

(** {1 Over Divisible functor}

    Produces a {!module-type:Preface_specs.DECIDABLE} from a
    {!module-type:Preface_specs.DIVISIBLE}. *)

module Over_divisible
    (Divisible : Preface_specs.Divisible.CORE)
    (Req : Preface_specs.Decidable.WITH_LOSE_AND_CHOOSE
             with type 'a t = 'a Divisible.t) :
  Preface_specs.DECIDABLE with type 'a t = 'a Req.t

(** {1 Manual construction}

    Advanced way to build an {!module-type:Preface_specs.DECIDABLE},
    constructing and assembling a component-by-component of
    {!module-type:Preface_specs.DECIDABLE}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Decidable.CORE)
    (Operation : Preface_specs.Decidable.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Decidable.INFIX with type 'a t = 'a Operation.t) :
  Preface_specs.DECIDABLE with type 'a t = 'a Infix.t

(** {2 Building Core} *)

module Core_via_divide_and_conquer
    (Divisible_req : Preface_specs.Divisible.WITH_DIVIDE_AND_CONQUER)
    (Req : Preface_specs.Decidable.WITH_LOSE_AND_CHOOSE
             with type 'a t = 'a Divisible_req.t) :
  Preface_specs.Decidable.CORE with type 'a t = 'a Req.t

module Core_via_contramap_and_divide_and_conquer
    (Divisible_req : Preface_specs.Divisible
                     .WITH_CONTRAMAP_AND_DIVIDE_AND_CONQUER)
    (Req : Preface_specs.Decidable.WITH_LOSE_AND_CHOOSE
             with type 'a t = 'a Divisible_req.t) :
  Preface_specs.Decidable.CORE with type 'a t = 'a Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Decidable.CORE) :
  Preface_specs.Decidable.OPERATION with type 'a t = 'a Core.t

(** {2 Deriving Infix} *)

module Infix
    (Core : Preface_specs.Decidable.CORE)
    (Operation : Preface_specs.Decidable.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Decidable.INFIX with type 'a t = 'a Operation.t
