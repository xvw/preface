(** TODO
*)

(** {1 Structure anatomy} *)

(** Standard requirement. *)
module CORE : sig
  type 'a data

  type _ t =
    | Return : 'a -> 'a t
    | Bind : 'b data * ('b -> 'a t) -> 'a t
        (** The type holded by the [Free]. *)
end

(** {1 API} *)

module type API = module type of CORE
(** The complete interface of a [Functor]. *)
