(** Exposes [Predicate.t], a function from ['a] to [bool].

    {1 Capabilities}

    - {!val:Contravariant} *)

(** {1 Type} *)

type 'a t

(** {1 Implementation} *)

module Contravariant : Preface_specs.CONTRAVARIANT with type 'a t = 'a t
(** {2 Foldable API} *)

(** {1 Helpers} *)

val lift : ('a -> bool) -> 'a t
(** Lift a function from ['a] to [bool] into a predicate. *)

val run : 'a t -> 'a -> bool
(** Run a predicate. *)
