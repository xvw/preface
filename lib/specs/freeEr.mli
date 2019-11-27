(** TODO
*)

(** {1 Structure anatomy} *)

(** Standard requirement. *)
module CORE (Type : sig
  type 'a t
end) : sig
  type _ t =
    | Return : 'a -> 'a t
    | Bind : 'b Type.t * ('b -> 'a t) -> 'a t
        (** The type holded by the [Free]. *)
end

(** {1 API} *)

module type API = module type of CORE
(** The complete interface of a [Functor]. *)
