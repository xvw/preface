(** [Void] describes unhabited type. A type that is not representable. *)

type t
(** Expressing a value of this type is impossible. *)

val absurd : t -> 'a
(** The law according to which any statement can be proven from a contradiction.
    That is, once a contradiction has been asserted, any proposition (including
    their negations) can be inferred from it; this is known as deductive
    explosion.*)

(** {1 As an identity for Either}

    [Void.t] act as an identity for [Either]. *)

val left : ('a, t) Stdlib.Either.t -> 'a
(** If there is [void] on the right part of [Either], we know that the Either is
    in [Left] branch. *)

val right : (t, 'b) Stdlib.Either.t -> 'b
(** If there is [void] on the left part of [Either], we know that the Either is
    in [Right] branch. *)
