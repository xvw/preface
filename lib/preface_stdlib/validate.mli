(** Exposes [Validate.t]; a biaised version of [Validation.t] with the errors
    fixed as a non empty list of exceptions.

    - {!val:Functor}
    - {!val:Applicative}
    - {!val:Selective}
    - {!val:Monad} *)

(** {1 Type} *)

type 'a t = ('a, exn Nonempty_list.t) Validation.t

(** {1 Implementation} *)

module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t
(** {2 Functor API} *)

module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t
(** {2 Applicative API} *)

module Selective : Preface_specs.SELECTIVE with type 'a t = 'a t
(** {2 Applicative API} *)

module Monad : Preface_specs.MONAD with type 'a t = 'a t
(** {2 Monad API} *)

(** {1 Helpers} *)

val pure : 'a -> 'a t
(** Create a value from ['a] to ['a t]. *)

val valid : 'a -> 'a t
(** Wrap a value into [Valid].*)

val invalid : exn Nonempty_list.t -> 'a t
(** Wrap an exception into [Invalid]. *)

val case : ('a -> 'b) -> (exn Nonempty_list.t -> 'b) -> 'a t -> 'b
(** [case f g x] apply [f] if [x] is [Valid], [g] if [x] is [Invalid].*)

val to_result : 'a t -> ('a, exn Nonempty_list.t) result
(** Project a [Validate] into a [Result]. *)

val eq : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Equality.*)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** Pretty printing. *)
