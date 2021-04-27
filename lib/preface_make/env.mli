(** Building a {!module:Preface_specs.Env}, an [Env transformer]. *)

(** {1 Using the minimal definition}

    Build an {!module-type:Preface_specs.ENV} over an {e Inner}
    {!module-type:Preface_specs.COMONAD}. *)

module Over_comonad (C : Preface_specs.COMONAD) (Env : Preface_specs.Types.T0) :
  Preface_specs.ENV with type env = Env.t and type 'a comonad = 'a C.t

(** {1 Improving API}

    If there are complementary implementations to
    {!module-type:Preface_specs.COMONAD} attached to the {e Inner}
    {!module-type:Preface_specs.COMONAD} type, it is possible to promote the
    {!module-type:Preface_specs.ENV} API with these complementary
    implementations *)

(** {2 Functor}

    If the {e Inner} {!module-type:Preface_specs.COMONAD} is also a
    {!module-type:Preface_specs.FUNCTOR}, the [Env comonad] is also a
    {!module-type:Preface_specs.FUNCTOR}. *)

module Functor (F : Preface_specs.FUNCTOR) (Env : Preface_specs.Types.T0) :
  Preface_specs.FUNCTOR with type 'a t = Env.t * 'a F.t

(** {2 Applicative}

    If the {e Inner} {!module-type:Preface_specs.COMONAD} is also an
    {!module-type:Preface_specs.APPLICATIVE} and the [Env] is a
    {!module-type:Preface_specs.MONOID} , the [Env comonad] is also a
    {!module-type:Preface_specs.APPLICATIVE}. *)

module Applicative (A : Preface_specs.APPLICATIVE) (Env : Preface_specs.MONOID) :
  Preface_specs.APPLICATIVE with type 'a t = Env.t * 'a A.t

(** {2 Comonad}

    If the {e Inner} {!module-type:Preface_specs.COMONAD} is also a
    {!module-type:Preface_specs.COMONAD}, the [Env comonad] is also a
    {!module-type:Preface_specs.COMONAD} (this is this functor which is used in
    the full API definition). *)

module Comonad (C : Preface_specs.COMONAD) (Env : Preface_specs.Types.T0) :
  Preface_specs.COMONAD with type 'a t = Env.t * 'a C.t

(** {1 Manual construction}

    Advanced way to build an {!module-type:Preface_specs.ENV}, constructing and
    assembling a component-by-component of {!module-type:Preface_specs.ENV}. (In
    order to provide your own implementation for some features.) *)

(** {2 Building Core} *)

module Core_over_comonad
    (C : Preface_specs.COMONAD)
    (Env : Preface_specs.Types.T0) :
  Preface_specs.Env.CORE with type env = Env.t and type 'a comonad = 'a C.t
