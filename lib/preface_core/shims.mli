(** Some Hook over the standard library. *)

(** {1 Hook over Either} *)

module Either : sig
  include module type of Either

  val case : ('a -> 'c) -> ('b -> 'c) -> ('a, 'b) t -> 'c
  (** [case f g x] conditionally perform [f] or [g] on [x]. *)
end
with type ('a, 'b) t = ('a, 'b) Either.t
