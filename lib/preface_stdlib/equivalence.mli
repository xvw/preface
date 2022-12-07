(** Implementation for [Equivalence.t]. *)

(** An [Equivalence] is just a function that takes to ['a] and return a [bool]. *)

(** {1 Type} *)

type 'a t = 'a -> 'a -> bool

(** {1 Implementation} *)

(** {2 Contravariant} *)

module Contravariant : Preface_specs.CONTRAVARIANT with type 'a t = 'a t

(** {2 Divisible} *)

module Divisible : Preface_specs.DIVISIBLE with type 'a t = 'a t

(** {2 Decidable} *)

module Decidable : Preface_specs.DECIDABLE with type 'a t = 'a t

(** {2 Invariant Functor} *)

module Invariant : Preface_specs.INVARIANT with type 'a t = 'a t

(** {1 Additional functions}

    Additional functions to facilitate practical work with [Equivalence.t]. *)

val negate : 'a t -> 'a t
(** negate the equivalence. *)
