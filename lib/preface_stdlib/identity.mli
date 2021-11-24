(** Implementation for [Identity.t]. *)

(** [Identity.t] is a trivial "type constructor" that allows you to lift an
    arbitrary value into a context. The identity can be very useful for
    validating implementations (by checking its laws) or for providing a fixed
    implementation for transformers. *)

(** {1 Type} *)

type 'a t
(** An identity is defined as [type 'a t = 'a].*)

(** {1 Implementation}

    The set of concrete implementations for [Identity.t]. *)

(** {2 Functor} *)

module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t

(** {2 Applicative} *)

module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t

(** {2 Selective} *)

module Selective : Preface_specs.SELECTIVE with type 'a t = 'a t

(** {2 Monad} *)

module Monad : Preface_specs.MONAD with type 'a t = 'a t

(** {2 Comonad} *)

module Comonad : Preface_specs.COMONAD with type 'a t = 'a t

(** {2 Invariant Functor} *)

module Invariant : Preface_specs.INVARIANT with type 'a t = 'a t

(** {1 Addtional functions}

    Additional functions to facilitate practical work with [Identity.t]. *)

val pure : 'a -> 'a t
(** Create a value from ['a] to ['a Identity.t]. *)

val extract : 'a t -> 'a
(** Extract the value from an [Identity.t]. *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Equality between [Identity.t].*)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** Formatter for pretty-printing for [Identity.t]. *)
