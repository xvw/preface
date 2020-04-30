(** Signatures representing types. Mainly used as a functor parameter *)

module type T0 = sig
  type t
end

module type T1 = sig
  type 'a t
end
