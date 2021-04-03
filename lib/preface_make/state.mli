(** Modules for building {!Preface_specs.STATE} modules. *)

(** {1 Documentation} *)

(** {1 Construction}

    Standard way to build a [State Monad]. *)

(** Given a [Monad] and a [State] produces a [State monad] over the state and
    using the monad as an inner monad. *)
module Over_monad (M : Preface_specs.MONAD) (State : Preface_specs.Types.T0) :
  Preface_specs.STATE with type state = State.t and type 'a monad = 'a M.t

(** {1 Improving API} *)

(** Incarnation of a [Functor] over a [State Monad]. *)
module Functor (F : Preface_specs.FUNCTOR) (State : Preface_specs.Types.T0) :
  Preface_specs.FUNCTOR with type 'a t = State.t -> ('a * State.t) F.t

(** Incarnation of an [Applicative] over a [State Monad]. *)
module Applicative (M : Preface_specs.MONAD) (State : Preface_specs.Types.T0) :
  Preface_specs.APPLICATIVE with type 'a t = State.t -> ('a * State.t) M.t

(** Incarnation of a [Monad] over a [State Monad]. *)
module Monad (M : Preface_specs.MONAD) (State : Preface_specs.Types.T0) :
  Preface_specs.MONAD with type 'a t = State.t -> ('a * State.t) M.t

(** Incarnation of a [Monad plus] over a [State Monad]. *)
module Monad_plus
    (M : Preface_specs.MONAD_PLUS)
    (State : Preface_specs.Types.T0) :
  Preface_specs.MONAD_PLUS with type 'a t = State.t -> ('a * State.t) M.t

(** Incarnation of an [Alternative] over a [State Monad]. *)
module Alternative
    (M : Preface_specs.MONAD_PLUS)
    (State : Preface_specs.Types.T0) :
  Preface_specs.ALTERNATIVE with type 'a t = State.t -> ('a * State.t) M.t

(** {2 Manual construction}

    Advanced way to build a [State Monad], constructing and assembling a
    component-by-component a monad. (In order to provide your own implementation
    for some features.) *)

(** Incarnation of the core of a [State Monad] over a [Monad] and a [State]. *)
module Core_over_monad
    (M : Preface_specs.MONAD)
    (State : Preface_specs.Types.T0) :
  Preface_specs.State.CORE with type state = State.t and type 'a monad = 'a M.t
