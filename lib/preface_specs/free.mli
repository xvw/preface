(** TODO *)

(** {1 Structure anatomy} *)

(** Standard requirement. *)
module CORE : sig
  module F : Functor.CORE

  (** The type holded by [Free]. *)
  type 'a t =
    | Return : 'a -> 'a t
    | Bind : 'a t F.t -> 'a t

  val eta : 'a F.t -> 'a t
  (** Lifting function from [F.t] to [t] *)
end

(** {1 API} *)

module type API = module type of CORE
(** The complete interface of a [Functor]. *)
