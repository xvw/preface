(** {1 Signature} *)

module type LAWS = sig
  module Functor : Preface_specs.FUNCTOR

  val preserve_identity_morphisms : unit -> ('a Functor.t, 'a Functor.t) Law.t
  (** Generates the law: [map id = id]. *)

  val preserve_composition_of_morphisms :
    unit -> ('a -> 'b, ('c -> 'a) -> 'c Functor.t -> 'b Functor.t) Law.t
  (** Generates the law: [map (f % g) = (map f) % (map g)]. *)
end

(** {1 Building}

    Construct the set of functions verifying the laws for a given functor. *)

module For (F : Preface_specs.FUNCTOR) : LAWS with module Functor := F
