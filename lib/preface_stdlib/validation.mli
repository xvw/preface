(** Exposes [Validation.t], a kind of [Result.t] but where the [Applicative]
    associates the error side with a [Semigroup] in order to accumulate errors.
    See [Validate] in order to see a biased version of [Validation].

    {1 Capabilities}

    - {!val:Bifunctor}
    - {!val:Functor} where ['a] of [('a, 'b) t] is delayed
    - {!val:Applicative} where ['a] of [('a, 'b) t] is delayed
    - {!val:Selective} where ['a] of [('a, 'b) t] is delayed
    - {!val:Monad} where ['a] of [('a, 'b) t] is delayed *)

(** {1 Type} *)

type ('a, 'errors) t =
  | Valid of 'a
  | Invalid of 'errors

(** {1 Implementation} *)

module Bifunctor : Preface_specs.BIFUNCTOR with type ('a, 'b) t = ('a, 'b) t
(** {2 Bifunctor API} *)

(** {2 Functor API} *)
module Functor (T : Preface_specs.Types.T0) :
  Preface_specs.FUNCTOR with type 'a t = ('a, T.t) t

(** {2 Applicative API} *)
module Applicative (Errors : Preface_specs.SEMIGROUP) :
  Preface_specs.Traversable.API_OVER_APPLICATIVE
    with type 'a t = ('a, Errors.t) t

(** {2 Selective API} *)
module Selective (Errors : Preface_specs.SEMIGROUP) :
  Preface_specs.SELECTIVE with type 'a t = ('a, Errors.t) t

(** {2 Monad API} *)
module Monad (T : Preface_specs.Types.T0) :
  Preface_specs.Traversable.API_OVER_MONAD with type 'a t = ('a, T.t) t

(** {1 Helpers} *)

val valid : 'a -> ('a, 'b) t
(** Wrap a value into [Valid].*)

val invalid : 'b -> ('a, 'b) t
(** Wrap an error value [Invalid]. *)

val pure : 'a -> ('a, 'b) t
(** Alias for [valid]. *)

val case : ('a -> 'c) -> ('b -> 'c) -> ('a, 'b) t -> 'c
(** [case f g x] apply [f] if [x] is [Valid], [g] if [x] is [Invalid].*)

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
