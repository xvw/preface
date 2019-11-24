(** TODO
*)

(** {1 Structure anatomy} *)

(** Standard requirement. *)
module CORE_with_type (Type : sig
  type 'a t
end) : sig
  type 'a t =
    | Return of 'a
    | FlatMap of 'a t Type.t
end

module type CORE = module type of CORE_with_type

(** {1 API} *)

module type API = CORE
(** The complete interface of a [Functor]. *)
