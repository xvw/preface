(** [Void] describes unhabited type. A type that is not representable. *)

type t
(** Expressing a value of this type is impossible. *)

val absurd : t -> 'a
(** The law according to which any statement can be proven from a contradiction.
    That is, once a contradiction has been asserted, any proposition (including
    their negations) can be inferred from it; this is known as deductive
    explosion.*)
