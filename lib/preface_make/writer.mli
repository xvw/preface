(** Modules for building {!Preface_specs.WRITER} modules. *)

(** {1 Documentation} *)

(** {1 Construction}

    Standard way to build a [Writer Monad]. *)

(** Given a [Monad] and a [Monoid] produces a [Writer monad] over the monoid and
    using the monad as an inner monad. *)
module Over_monad (M : Preface_specs.MONAD) (Tape : Preface_specs.MONOID) :
  Preface_specs.WRITER with type tape = Tape.t and type 'a monad = 'a M.t

(** {1 Improving API} *)

(** Incarnation of a [Functor] over a [Writer Monad]. *)
module Functor (F : Preface_specs.FUNCTOR) (Tape : Preface_specs.MONOID) :
  Preface_specs.FUNCTOR with type 'a t = ('a * Tape.t) F.t

(** Incarnation of an [Applicative] over a [Writer Monad]. *)
module Applicative (A : Preface_specs.APPLICATIVE) (Tape : Preface_specs.MONOID) :
  Preface_specs.APPLICATIVE with type 'a t = ('a * Tape.t) A.t

(** Incarnation of an [Alternative] over a [Writer Monad]. *)
module Alternative (A : Preface_specs.ALTERNATIVE) (Tape : Preface_specs.MONOID) :
  Preface_specs.ALTERNATIVE with type 'a t = ('a * Tape.t) A.t

(** Incarnation of a [Monad] over a [Writer Monad]. *)
module Monad (M : Preface_specs.MONAD) (Tape : Preface_specs.MONOID) :
  Preface_specs.MONAD with type 'a t = ('a * Tape.t) M.t

(** Incarnation of a [Monad plus] over a [Writer Monad]. *)
module Monad_plus (M : Preface_specs.MONAD_PLUS) (Tape : Preface_specs.MONOID) :
  Preface_specs.MONAD_PLUS with type 'a t = ('a * Tape.t) M.t

(** {2 Manual construction}

    Advanced way to build a [Writer Monad], constructing and assembling a
    component-by-component a monad. (In order to provide your own implementation
    for some features.) *)

(** Incarnation of the core of a [Writer Monad] over a [Monad] and a [Monoid]. *)
module Core_over_monad
    (Monad : Preface_specs.MONAD)
    (Tape : Preface_specs.MONOID) :
  Preface_specs.Writer.CORE
    with type tape = Tape.t
     and type 'a monad = 'a Monad.t
