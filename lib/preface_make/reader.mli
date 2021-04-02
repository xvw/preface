(** Modules for building {!Preface_specs.READER} modules. *)

(** {1 Documentation} *)

(** {1 Construction}

    Standard way to build a [Reader Monad]. *)

(** Given a [Monad] and an [Environment] produces a [Reader monad] over the
    environment and using the monad as an inner monad. *)
module Over_monad (M : Preface_specs.MONAD) (Env : Preface_specs.Types.T0) :
  Preface_specs.READER with type env = Env.t and type 'a monad = 'a M.t

(** {1 Improving API} *)

(** Incarnation of a [Functor] over a [Reader Monad]. *)
module Functor (F : Preface_specs.FUNCTOR) (Env : Preface_specs.Types.T0) :
  Preface_specs.FUNCTOR with type 'a t = Env.t -> 'a F.t

(** Incarnation of an [Applicative] over a [Reader Monad]. *)
module Applicative
    (A : Preface_specs.APPLICATIVE)
    (Env : Preface_specs.Types.T0) :
  Preface_specs.APPLICATIVE with type 'a t = Env.t -> 'a A.t

(** Incarnation of an [Alternative] over a [Reader Monad]. *)
module Alternative
    (A : Preface_specs.ALTERNATIVE)
    (Env : Preface_specs.Types.T0) :
  Preface_specs.ALTERNATIVE with type 'a t = Env.t -> 'a A.t

(** Incarnation of a [Monad] over a [Reader Monad]. *)
module Monad (M : Preface_specs.MONAD) (Env : Preface_specs.Types.T0) :
  Preface_specs.MONAD with type 'a t = Env.t -> 'a M.t

(** Incarnation of a [Monad plus] over a [Reader Monad]. *)
module Monad_plus (M : Preface_specs.MONAD_PLUS) (Env : Preface_specs.Types.T0) :
  Preface_specs.MONAD_PLUS with type 'a t = Env.t -> 'a M.t

(** {2 Manual construction}

    Advanced way to build a [Reader Monad], constructing and assembling a
    component-by-component a monad. (In order to provide your own implementation
    for some features.) *)

(** Incarnation of the core of a [Reader Monad] over a [Monad] and an
    [Environment]. *)
module Core_over_monad
    (Monad : Preface_specs.MONAD)
    (Env : Preface_specs.Types.T0) :
  Preface_specs.Reader.CORE with type 'a monad = 'a Monad.t and type env = Env.t
