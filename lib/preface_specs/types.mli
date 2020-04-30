(** Basic types definition mainly used by [Freer_monad] and [Either] *)

module type T0 = sig
  type t
end

module type T1 = sig
  type 'a t
end
