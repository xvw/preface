(** Exposes [State.t].
    {1 Capabilities}

    - {!val:Functor}
    - {!val:Applicative}
    - {!val:Monad} *)

module Via_type (T : sig
  type t
end) : sig
  type state = T.t

  type 'a t = state -> 'a * state

  module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t
  (** {2 Functor API} *)

  module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t
  (** {2 Applicative API} *)

  module Monad : Preface_specs.MONAD with type 'a t = 'a t
  (** { 2 Monad API} *)

  val run : state -> 'a t -> 'a * state
  (** TODO *)

  val get : state t
  (** TODO *)

  val set : state -> unit t
  (** TODO *)

  val modify : (state -> state) -> unit t
  (** TODO *)
end
