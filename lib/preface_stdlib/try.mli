(** Implementation for [Try.t]. *)

(** [Try.t] is a biased version of [Result] with the error fixed as an
    [exception]. *)

(** {1 Type} *)

type 'a t = ('a, exn) result

(** {1 Implementation} *)

(** {2 Functor} *)

module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t

(** {2 Alt} *)

module Alt : Preface_specs.ALT with type 'a t = 'a t

(** {2 Applicative}

    [Try.t] implements {!module-type:Preface_specs.APPLICATIVE} and introduces
    an interface to define {!module-type:Preface_specs.TRAVERSABLE} using [Try]
    as an iterable structure. *)

module Applicative :
  Preface_specs.Traversable.API_OVER_APPLICATIVE with type 'a t = 'a t

(** {2 Monad}

    [Try.t] implements {!module-type:Preface_specs.MONAD} and introduces an
    interface to define {!module-type:Preface_specs.TRAVERSABLE} using [Try] as
    an iterable structure. *)

module Monad : Preface_specs.Traversable.API_OVER_MONAD with type 'a t = 'a t

(** {2 Foldable} *)

module Foldable : Preface_specs.FOLDABLE with type 'a t = 'a t

(** {1 Addtional functions}

    Additional functions to facilitate practical work with [Try.t]. *)

val pure : 'a -> 'a t
(** Create a value from ['a] to ['a t]. *)

val ok : 'a -> 'a t
(** Wrap a value into [Ok].*)

val error : exn -> 'a t
(** Wrap an exception into [Error]. *)

val capture : (unit -> 'a) -> 'a t
(** [capture f] perform [f] and wrap the result into a [t] if the execution of
    [f] raise no exception, the result will be [Ok result] else, the catched
    exception if wrapped into [Error exn]. *)

val case : ('a -> 'b) -> (exn -> 'b) -> 'a t -> 'b
(** [case f g x] apply [f] if [x] is [Ok], [g] if [x] is [Error].*)

(* I do not use ['a Validation.t] as type to not have circular deps *)
val to_validation : 'a t -> ('a, exn Nonempty_list.t) Validation.t
(** Project a [Try] into a [Validation]. *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Equality between [Try.t].*)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** Formatter for pretty-printing for [Try.t]. *)
