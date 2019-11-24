(** TODO
*)

(** {1 Structure anatomy} *)

(** Standard requirement. *)
module CORE_with_type (Type : sig
  type 'a t
end) : sig
  type _ t =
    | Return : 'a -> 'a t
    | FlatMap : 'b Type.t * ('b -> 'a t) -> 'a t
        (** The type holded by the [Free]. *)
end

module type CORE = module type of CORE_with_type

(** {1 API} *)

module type API = CORE
(** The complete interface of a [Functor]. *)
