(** Some helpers over [Either.t] *)

val swap : ('a, 'b) Stdlib.Either.t -> ('b, 'a) Stdlib.Either.t
(** [swap x] transforms [Left x] into [Right x] and [ Right x] into [Left x] *)
