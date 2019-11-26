(** Modules for building [Freeer] modules. *)

module Make_freeer_functor (F : sig
  type 'a t
end) : Specs.Functor.CORE
