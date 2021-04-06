(** Building a {!module:Preface_specs.Reader}, a [Reader transformer]. *)

(** {1 Using the minimal definition}

    Build an {!module-type:Preface_specs.READER} over an {e Inner}
    {!module-type:Preface_specs.MONAD} and given an {e environment}. *)

module Over_monad (M : Preface_specs.MONAD) (Env : Preface_specs.Types.T0) :
  Preface_specs.READER with type env = Env.t and type 'a monad = 'a M.t

(** {1 Improving API}

    If there are complementary implementations to
    {!module-type:Preface_specs.MONAD} attached to the {e Inner}
    {!module-type:Preface_specs.MONAD} type, it is possible to promote the
    {!module-type:Preface_specs.READER} API with these complementary
    implementations *)

(** {2 Functor}

    If the {e Inner} {!module-type:Preface_specs.MONAD} is also a
    {!module-type:Preface_specs.FUNCTOR}, the [Reader monad] is also a
    {!module-type:Preface_specs.FUNCTOR}. *)

module Functor (F : Preface_specs.FUNCTOR) (Env : Preface_specs.Types.T0) :
  Preface_specs.FUNCTOR with type 'a t = Env.t -> 'a F.t

(** {2 Applicative}

    If the {e Inner} {!module-type:Preface_specs.MONAD} is also an
    {!module-type:Preface_specs.APPLICATIVE}, the [Reader monad] is also an
    {!module-type:Preface_specs.APPLICATIVE}. *)

module Applicative
    (A : Preface_specs.APPLICATIVE)
    (Env : Preface_specs.Types.T0) :
  Preface_specs.APPLICATIVE with type 'a t = Env.t -> 'a A.t

(** {2 Alternative}

    If the {e Inner} {!module-type:Preface_specs.MONAD} is also an
    {!module-type:Preface_specs.ALTERNATIVE}, the [Reader monad] is also an
    {!module-type:Preface_specs.ALTERNATIVE}. *)

module Alternative
    (A : Preface_specs.ALTERNATIVE)
    (Env : Preface_specs.Types.T0) :
  Preface_specs.ALTERNATIVE with type 'a t = Env.t -> 'a A.t

(** {2 Monad}

    If the {e Inner} {!module-type:Preface_specs.MONAD} is also a
    {!module-type:Preface_specs.MONAD}, the [Reader monad] is also a
    {!module-type:Preface_specs.MONAD} (this is this functor which is used in
    the full API definition). *)

module Monad (M : Preface_specs.MONAD) (Env : Preface_specs.Types.T0) :
  Preface_specs.MONAD with type 'a t = Env.t -> 'a M.t

(** {2 Monad Plus}

    If the {e Inner} {!module-type:Preface_specs.MONAD} is also a
    {!module-type:Preface_specs.MONAD_PLUS}, the [Reader monad] is also a
    {!module-type:Preface_specs.MONAD_PLUS}. *)

module Monad_plus (M : Preface_specs.MONAD_PLUS) (Env : Preface_specs.Types.T0) :
  Preface_specs.MONAD_PLUS with type 'a t = Env.t -> 'a M.t

(** {1 Manual construction}

    Advanced way to build an {!module-type:Preface_specs.READER}, constructing
    and assembling a component-by-component of
    {!module-type:Preface_specs.READER}. (In order to provide your own
    implementation for some features.) *)

(** {2 Building Core} *)

module Core_over_monad
    (Monad : Preface_specs.MONAD)
    (Env : Preface_specs.Types.T0) :
  Preface_specs.Reader.CORE with type 'a monad = 'a Monad.t and type env = Env.t
