(** Building a {!module:Preface_specs.Writer}, a [Writer transformer]. *)

(** {1 Using the minimal definition}

    Build an {!module-type:Preface_specs.WRITER} over an {e Inner}
    {!module-type:Preface_specs.MONAD} and given a {e monoidal tape}. *)

module Over_monad (M : Preface_specs.MONAD) (Tape : Preface_specs.MONOID) :
  Preface_specs.WRITER with type tape = Tape.t and type 'a monad = 'a M.t

(** {1 Improving API}

    If there are complementary implementations to
    {!module-type:Preface_specs.MONAD} attached to the {e Inner}
    {!module-type:Preface_specs.MONAD} type, it is possible to promote the
    {!module-type:Preface_specs.WRITER} API with these complementary
    implementations *)

(** {2 Functor}

    If the {e Inner} {!module-type:Preface_specs.MONAD} is also a
    {!module-type:Preface_specs.FUNCTOR}, the [Writer monad] is also a
    {!module-type:Preface_specs.FUNCTOR}. *)

module Functor (F : Preface_specs.FUNCTOR) (Tape : Preface_specs.MONOID) :
  Preface_specs.FUNCTOR with type 'a t = ('a * Tape.t) F.t

(** {2 Applicative}

    If the {e Inner} {!module-type:Preface_specs.MONAD} is also an
    {!module-type:Preface_specs.APPLICATIVE}, the [Writer monad] is also an
    {!module-type:Preface_specs.APPLICATIVE}. *)

module Applicative (A : Preface_specs.APPLICATIVE) (Tape : Preface_specs.MONOID) :
  Preface_specs.APPLICATIVE with type 'a t = ('a * Tape.t) A.t

(** {2 Alternative}

    If the {e Inner} {!module-type:Preface_specs.MONAD} is also an
    {!module-type:Preface_specs.ALTERNATIVE}, the [Writer monad] is also an
    {!module-type:Preface_specs.ALTERNATIVE}. *)

module Alternative (A : Preface_specs.ALTERNATIVE) (Tape : Preface_specs.MONOID) :
  Preface_specs.ALTERNATIVE with type 'a t = ('a * Tape.t) A.t

(** {2 Monad}

    If the {e Inner} {!module-type:Preface_specs.MONAD} is also a
    {!module-type:Preface_specs.MONAD}, the [Writer monad] is also a
    {!module-type:Preface_specs.MONAD} (this is this functor which is used in
    the full API definition). *)

module Monad (M : Preface_specs.MONAD) (Tape : Preface_specs.MONOID) :
  Preface_specs.MONAD with type 'a t = ('a * Tape.t) M.t

(** {2 Monad Plus}

    If the {e Inner} {!module-type:Preface_specs.MONAD} is also a
    {!module-type:Preface_specs.MONAD_PLUS}, the [Writer monad] is also a
    {!module-type:Preface_specs.MONAD_PLUS}. *)

module Monad_plus (M : Preface_specs.MONAD_PLUS) (Tape : Preface_specs.MONOID) :
  Preface_specs.MONAD_PLUS with type 'a t = ('a * Tape.t) M.t

(** {1 Manual construction}

    Advanced way to build an {!module-type:Preface_specs.WRITER}, constructing
    and assembling a component-by-component of
    {!module-type:Preface_specs.WRITER}. (In order to provide your own
    implementation for some features.) *)

(** {2 Building Core} *)

module Core_over_monad
    (Monad : Preface_specs.MONAD)
    (Tape : Preface_specs.MONOID) :
  Preface_specs.Writer.CORE
    with type tape = Tape.t
     and type 'a monad = 'a Monad.t
