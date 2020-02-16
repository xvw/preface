(** Exposes [Try.t]

    {1 Capabilities}

    - {!val:Functor}
    - {!val:Applicative}
    - {!val:Monad}

    {1 Use cases}

    [Try] allows you to create sequential validation pipelines. Unlike
    [Validation] errors are not accumulate. The pipelines returns [Ok result] or
    [Error first_error_occured]. [Try] goes well with monadic composition.

    {2 Sequential pipelining}

    [Try] is usually useful when a collection of functions can fail and we would
    like to sequence them. For example :

    {[
      exception Invalid_age of int

      exception Invalid_name of string

      let validate_age age =
        if age >= 0 then Ok age else Error (Invalid_age age)
      ;;

      let validate_name name =
        if String.length name >= 2 then Ok name else Error (Invalid_name name)
      ;;
    ]}

    We want, if the age is valid, to verify that the name is valid, and if the
    name is also valid, to produce the following couple: [age, name].

    {[
      let result in_age in_name =
        let open Preface.Try.Monad.Infix in
        validate_age in_age
        >>= (fun age -> validate_name in_name >|= (fun name -> (age, name)))
      ;;
    ]}

    Or using the let operators:

    {[
      let result in_age in_name =
        let open Preface.Try.Monad.Syntax in
        let* age = validate_age in_age in
        let+ name = validate_name in_name in
        (age, name)
      ;;
    ]} *)

(** {1 Type} *)

type 'a t = ('a, exn) result

(** {1 Implementation} *)

module Functor : Preface_specs.FUNCTOR with type 'a t := 'a t
(** {2 Functor API} *)

module Applicative : Preface_specs.APPLICATIVE with type 'a t := 'a t
(** {2 Applicative API} *)

module Monad : Preface_specs.MONAD with type 'a t = 'a t
(** {2 Monad API} *)

(** {1 Helpers} *)

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
val to_validation : 'a t -> ('a, exn list) result
(** Project a [Try] into a [Validation]. *)

val eq : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Equality.*)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** Pretty printing. *)
