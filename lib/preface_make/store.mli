(** Building a {!module:Preface_specs.Store}, a [Store transformer]. *)

(** {1 Using the minimal definition}

    Build an {!module-type:Preface_specs.STORE} over an {e Inner}
    {!module-type:Preface_specs.COMONAD}. *)

module Over_comonad (C : Preface_specs.COMONAD) (Store : Preface_specs.Types.T0) :
  Preface_specs.STORE with type store = Store.t and type 'a comonad = 'a C.t

(** {1 Improving API}

    If there are complementary implementations to
    {!module-type:Preface_specs.COMONAD} attached to the {e Inner}
    {!module-type:Preface_specs.COMONAD} type, it is possible to promote the
    {!module-type:Preface_specs.STORE} API with these complementary
    implementations *)

(** {2 Functor}

    If the {e Inner} {!module-type:Preface_specs.COMONAD} is also a
    {!module-type:Preface_specs.FUNCTOR}, the [Store comonad] is also a
    {!module-type:Preface_specs.FUNCTOR}. *)

module Functor (F : Preface_specs.FUNCTOR) (Store : Preface_specs.Types.T0) :
  Preface_specs.FUNCTOR with type 'a t = (Store.t -> 'a) F.t * Store.t

(** {2 Comonad}

    If the {e Inner} {!module-type:Preface_specs.COMONAD} is also a
    {!module-type:Preface_specs.COMONAD}, the [Store comonad] is also a
    {!module-type:Preface_specs.COMONAD} (this is this functor which is used in
    the full API definition). *)

module Comonad (C : Preface_specs.COMONAD) (Store : Preface_specs.Types.T0) :
  Preface_specs.COMONAD with type 'a t = (Store.t -> 'a) C.t * Store.t

(** {1 Manual construction}

    Advanced way to build an {!module-type:Preface_specs.STORE}, constructing
    and assembling a component-by-component of
    {!module-type:Preface_specs.STORE}. (In order to provide your own
    implementation for some features.) *)

(** {2 Building Core} *)

module Core_over_comonad
    (C : Preface_specs.COMONAD)
    (Store : Preface_specs.Types.T0) :
  Preface_specs.Store.CORE
    with type store = Store.t
     and type 'a comonad = 'a C.t
