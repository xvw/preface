(** Building a {!module:Preface_specs.State}, a [State transformer]. *)

(** {1 Using the minimal definition}

    Build an {!module-type:Preface_specs.STATE} over an {e Inner}
    {!module-type:Preface_specs.MONAD}. *)

module Over_monad (M : Preface_specs.MONAD) (State : Preface_specs.Types.T0) :
  Preface_specs.STATE with type state = State.t and type 'a monad = 'a M.t

(** {1 Improving API}

    If there are complementary implementations to
    {!module-type:Preface_specs.MONAD} attached to the {e Inner}
    {!module-type:Preface_specs.MONAD} type, it is possible to promote the
    {!module-type:Preface_specs.STATE} API with these complementary
    implementations *)

(** {2 Functor}

    If the {e Inner} {!module-type:Preface_specs.MONAD} is also a
    {!module-type:Preface_specs.FUNCTOR}, the [State monad] is also a
    {!module-type:Preface_specs.FUNCTOR}. *)

module Functor (F : Preface_specs.FUNCTOR) (State : Preface_specs.Types.T0) :
  Preface_specs.FUNCTOR with type 'a t = State.t -> ('a * State.t) F.t

(** {2 Applicative}

    If the {e Inner} {!module-type:Preface_specs.MONAD} is also an
    {!module-type:Preface_specs.MONAD}, the [State monad] is also an
    {!module-type:Preface_specs.APPLICATIVE}. *)

module Applicative (M : Preface_specs.MONAD) (State : Preface_specs.Types.T0) :
  Preface_specs.APPLICATIVE with type 'a t = State.t -> ('a * State.t) M.t

(** {2 Alternative}

    If the {e Inner} {!module-type:Preface_specs.MONAD} is also an
    {!module-type:Preface_specs.MONAD_PLUS}, the [State monad] is also an
    {!module-type:Preface_specs.ALTERNATIVE}. *)

module Alternative
    (M : Preface_specs.MONAD_PLUS)
    (State : Preface_specs.Types.T0) :
  Preface_specs.ALTERNATIVE with type 'a t = State.t -> ('a * State.t) M.t

(** {2 Monad}

    If the {e Inner} {!module-type:Preface_specs.MONAD} is also a
    {!module-type:Preface_specs.MONAD}, the [State monad] is also a
    {!module-type:Preface_specs.MONAD} (this is this functor which is used in
    the full API definition). *)

module Monad (M : Preface_specs.MONAD) (State : Preface_specs.Types.T0) :
  Preface_specs.MONAD with type 'a t = State.t -> ('a * State.t) M.t

(** {2 Monad Plus}

    If the {e Inner} {!module-type:Preface_specs.MONAD} is also a
    {!module-type:Preface_specs.MONAD_PLUS}, the [State monad] is also a
    {!module-type:Preface_specs.MONAD_PLUS}. *)

module Monad_plus
    (M : Preface_specs.MONAD_PLUS)
    (State : Preface_specs.Types.T0) :
  Preface_specs.MONAD_PLUS with type 'a t = State.t -> ('a * State.t) M.t

(** {1 Manual construction}

    Advanced way to build an {!module-type:Preface_specs.STATE}, constructing
    and assembling a component-by-component of
    {!module-type:Preface_specs.STATE}. (In order to provide your own
    implementation for some features.) *)

(** {2 Building Core} *)

module Core_over_monad
    (M : Preface_specs.MONAD)
    (State : Preface_specs.Types.T0) :
  Preface_specs.State.CORE with type state = State.t and type 'a monad = 'a M.t
