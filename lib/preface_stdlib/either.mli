(** Exposes [Either.t]

    {1 Capabilities}

    - {!val:Bifunctor}
    - {!val:Functor} where ['a] of [('a, 'b) t] is delayed
    - {!val:Applicative} where ['a] of [('a, 'b) t] is delayed
    - {!val:Monad} where ['a] of [('a, 'b) t] is delayed *)

(** {1 Type} *)

type ('a, 'b) t = ('a, 'b) Preface_core.Either.t =
  | Left of 'a
  | Right of 'b

(** {1 Implementation} *)

module Bifunctor : Preface_specs.BIFUNCTOR with type ('a, 'b) t = ('a, 'b) t
(** {2 Bifunctor API} *)

(** {2 Functor API} *)
module Functor (T : Preface_specs.Types.T0) :
  Preface_specs.FUNCTOR with type 'a t = (T.t, 'a) Bifunctor.t

(** {2 Applicative API} *)
module Applicative (T : Preface_specs.Types.T0) :
  Preface_specs.APPLICATIVE with type 'a t = (T.t, 'a) Bifunctor.t

(** {2 Monad API} *)
module Monad (T : Preface_specs.Types.T0) :
  Preface_specs.MONAD with type 'a t = (T.t, 'a) Bifunctor.t

(** {1 Helpers} *)

val pure : 'b -> ('a, 'b) t
(** Create a value from ['b] to [('a, 'b) t]. *)

include Preface_specs.Requirements.EITHER with type ('a, 'b) t := ('a, 'b) t

val equal :
  ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
(** Equality. *)

val pp :
     (Format.formatter -> 'a -> unit)
  -> (Format.formatter -> 'b -> unit)
  -> Format.formatter
  -> ('a, 'b) t
  -> unit
(** Pretty printing. *)
