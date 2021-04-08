(** Implementation for [Continuation.t]. *)

(** {1 Type} *)

type 'a t = { run : 'r. ('a -> 'r) -> 'r }

(** {1 Implementation} *)

(** {2 Functor} *)

module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t

(** {2 Applicative} *)

module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t

(** {2 Monad} *)

module Monad : Preface_specs.MONAD with type 'a t = 'a t
(** {2 Monad API} *)

(** {1 Addtional functions}

    Additional functions to facilitate practical work with [Continuation.t]. *)

val pure : 'a -> 'a t
(** Create a value from ['a] to ['a t]. *)
