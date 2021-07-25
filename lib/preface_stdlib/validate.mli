(** Implementation for [Try.t]. *)

(** [Validate.t] is a biased version of [Validation] with the errors fixed as an
    [Nonempty_list] of [exception]. *)

(** {1 Type} *)

type 'a t = ('a, exn Nonempty_list.t) Validation.t

(** {2 About Validate}

    Validate is a specialisation of [Validation], so the module does not
    re-export the Validation constructors (they do not have the same arity) so
    if you want to pattern match a [Validate] value you will have to use the
    [Validation] constructors:

    {[
      match validated_value with
      | Validation.Valid x -> x
      | Validation.Invalid errors ->
        Format.sprintf "Error: %a" (Nonempty_list.pp Exn.pp) errors
    ]} *)

(** {1 Implementation} *)

(** {2 Functor} *)

module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t

(** {2 Alt} *)

module Alt : Preface_specs.ALT with type 'a t = 'a t

(** {2 Applicative}

    [Validate.t] implements {!module-type:Preface_specs.APPLICATIVE} and
    introduces an interface to define {!module-type:Preface_specs.TRAVERSABLE}
    using [Validate] as an iterable structure.

    As you can see, it is in the definition of the
    {!module-type:Preface_specs.APPLICATIVE} that [Validate] differs from [Try].
    The ['errors] part must be a {!module-type:Preface_specs.SEMIGROUP} to allow
    for the accumulation of errors. *)

module Applicative :
  Preface_specs.Traversable.API_OVER_APPLICATIVE with type 'a t = 'a t

(** {3 Selective} *)

module Selective : Preface_specs.SELECTIVE with type 'a t = 'a t

(** {3 Monad}

    [Validate.t] implements {!module-type:Preface_specs.MONAD} and introduces an
    interface to define {!module-type:Preface_specs.TRAVERSABLE} using
    [Validate] as an iterable structure. *)

module Monad : Preface_specs.Traversable.API_OVER_MONAD with type 'a t = 'a t

(** {2 Foldable} *)

module Foldable : Preface_specs.FOLDABLE with type 'a t = 'a t

(** {1 Addtional functions}

    Additional functions to facilitate practical work with [Validate.t]. *)

val pure : 'a -> 'a t
(** Create a value from ['a] to ['a t]. *)

val valid : 'a -> 'a t
(** Wrap a value into [Valid].*)

val invalid : exn Nonempty_list.t -> 'a t
(** Wrap an exception list into [Invalid]. *)

val error : exn -> 'a t
(** Wrap an exception into a list and wrap the list into [Invalid]. *)

val case : ('a -> 'b) -> (exn Nonempty_list.t -> 'b) -> 'a t -> 'b
(** [case f g x] apply [f] if [x] is [Valid], [g] if [x] is [Invalid].*)

val to_result : 'a t -> ('a, exn Nonempty_list.t) result
(** Project a [Validate] into a [Result]. *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Equality between [Validate.t].*)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** Formatter for pretty-printing for [Validate.t]. *)
