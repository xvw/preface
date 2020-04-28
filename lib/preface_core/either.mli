(** Exposes [Either.t] (mainly as a dependencie for Selective). *)

(** {1 Type} *)

type ('a, 'b) t =
  | Left of 'a
  | Right of 'b

(** {1 API} *)

include Requirements.EITHER with type ('a, 'b) t := ('a, 'b) t
