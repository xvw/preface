(** TODO *)

(** {1 Structure anatomy} *)

(** Standard requirement. *)
module CORE : sig
  type 'a g

  (** The type holded by [Freer]. *)
  type _ t =
    | Return : 'a -> 'a t
    | Bind : 'b g * ('b -> 'a t) -> 'a t

  val eta : 'a g -> 'a t
  (** Lifting function from [g] to [t] *)
end

(** {1 API} *)

module type API = module type of CORE
(** The complete interface of a [Functor]. *)
