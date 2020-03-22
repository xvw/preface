(** TODO *)

(** {1 Structure anatomy} *)

(** Standard requirement. *)
module CORE (Type : sig
  type 'a t
end) : sig
  type 'a t =
    | Return of 'a
    | Bind of 'a t Type.t
end

(** {1 API} *)

module type API = module type of CORE
(** The complete interface of a [Functor]. *)
