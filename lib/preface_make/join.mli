(** [Join] produces a [Functor] from a [Bifunctor] using both arguments of a
    Bifunctor. *)

(** {2 Produces a Functor from a Bifunctor} *)

module Functor (B : Preface_specs.Bifunctor.CORE) :
  Preface_specs.FUNCTOR with type 'a t = ('a, 'a) B.t
