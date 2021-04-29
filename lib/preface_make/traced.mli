(** Building a {!module:Preface_specs.Traced}, a [Traced transformer]. *)

(** {1 Using the minimal definition}

    Build an {!module-type:Preface_specs.TRACED} over an {e Inner}
    {!module-type:Preface_specs.COMONAD}. *)

module Over_comonad (C : Preface_specs.COMONAD) (Tape : Preface_specs.MONOID) :
  Preface_specs.TRACED with type tape = Tape.t and type 'a comonad = 'a C.t

(** {1 Improving API}

    If there are complementary implementations to
    {!module-type:Preface_specs.COMONAD} attached to the {e Inner}
    {!module-type:Preface_specs.COMONAD} type, it is possible to promote the
    {!module-type:Preface_specs.TRACED} API with these complementary
    implementations *)

(** {2 Functor}

    If the {e Inner} {!module-type:Preface_specs.COMONAD} is also a
    {!module-type:Preface_specs.FUNCTOR}, the [Traced comonad] is also a
    {!module-type:Preface_specs.FUNCTOR}. *)

module Functor (F : Preface_specs.FUNCTOR) (Tape : Preface_specs.MONOID) :
  Preface_specs.FUNCTOR with type 'a t = (Tape.t -> 'a) F.t

(** {2 Comonad}

    If the {e Inner} {!module-type:Preface_specs.COMONAD} is also a
    {!module-type:Preface_specs.COMONAD}, the [Traced comonad] is also a
    {!module-type:Preface_specs.COMONAD} (this is this functor which is used in
    the full API definition). *)

module Comonad (C : Preface_specs.COMONAD) (Tape : Preface_specs.MONOID) :
  Preface_specs.COMONAD with type 'a t = (Tape.t -> 'a) C.t

(** {1 Manual construction}

    Advanced way to build an {!module-type:Preface_specs.TRACED}, constructing
    and assembling a component-by-component of
    {!module-type:Preface_specs.TRACED}. (In order to provide your own
    implementation for some features.) *)

(** {2 Building Core} *)

module Core_over_comonad
    (C : Preface_specs.COMONAD)
    (Tape : Preface_specs.MONOID) :
  Preface_specs.Traced.CORE with type tape = Tape.t and type 'a comonad = 'a C.t
