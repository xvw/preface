(** Exposes [Validation.Make]

    {1 Capabilities}

    - {!val:Bifunctor}
    - {!val:Functor} where ['a] of [('a, 'b) t] is delayed
    - {!val:Applicative} where ['a] of [('a, 'b) t] is delayed
    - {!val:Selective} where ['a] of [('a, 'b) t] is delayed
    - {!val:Monad} where ['a] of [('a, 'b) t] is delayed

    {1 Use cases}

    [Validation] allows you to create validation pipelines that accumulate
    errors. Unlike [Try], which performs sequential validations, [Validation]
    goes well with the applicative composition.

    {2 A Formlet example}

    Formlet validation is a problem recognized as being complex. Fortunately, 20
    years ago, a method drawing on application functors was highlighted in the
    paper {{:http://homepages.inf.ed.ac.uk/slindley/papers/formlets-essence.pdf}
    "The essence of Form Abstraction"}.

    We suppose that a form contains several fields with which we associate
    different validation rules:

    - [age] must be positive;
    - [firstname] must have at least 2 characters;
    - [lastname] must have at least 2 characters;
    - [checked_rules], the "do you accept the rules of the website", which must
      be true.

    {[
      module Subscription_formlet = struct
        type t = {
            age : int
          ; firstname : string
          ; lastname : string
          ; checked_rules : bool
        }

        let make age firstname lastname checked_rules =
          { age; firstname; lastname; checked_rules }
        ;;
      end
    ]}

    We can now implement the validation functions and the corresponding
    exceptions to error cases.

    {[
      module Subscription_formlet = struct
        type t = {
            age : int
          ; firstname : string
          ; lastname : string
          ; checked_rules : bool
        }

        let make age firstname lastname checked_rules =
          { age; firstname; lastname; checked_rules }
        ;;

        exception Invalid_age of int

        exception Invalid_name of ([ `Firstname | `Lastname ] * string)

        exception Unchecked_rules

        let validate_age age =
          if age >= 0 then Valid age else Invalid [ Invalid_age age ]
        ;;

        let validate_name subject name =
          if String.length name > 1
          then Valid name
          else Invalid [ Invalid_name (subject, name) ]
        ;;

        let validate_rules checked =
          if checked then Valid checked else Invalid [ Unchecked_rules ]
        ;;
      end
    ]}

    We can now use our [make] functions and our validation functions to validate
    the creation of a subscription:

    {[
      module V =
        Preface.Validation.Applicative
          (struct
            type t = exn
          end)
          (Preface.List)

      let try_register age firstname lastname checked_rules =
        let open Subscription_formlet in
        let open V.Infix in
        make
        <$> validate_age age
        <*> validate_name `Firstname firstname
        <*> validate_name `Lastname lastname
        <*> validate_rules checked_rules
      ;;
    ]}

    Here is some usage example, when everything is fine:

    {[
      try_register 10 "Xavier" "Van de Woestyne" true;;
      # Valid
        {Subscription_formlet.age = 10; firstname = "Xavier";
         lastname = "Van de Woestyne"; checked_rules = true}
    ]}

    When the checkbox "Accept all the rules" is not checked!

    {[
      try_register 10 "Xavier" "Van de Woestyne" false;;
      # Invalid [Subscription_formlet.Unchecked_rules]
    ]}

    When the checkbox "Accept all the rules" is not checked and the [age] is
    negative!

    {[
      try_register (-5) "Xavier" "Van de Woestyne" false;;
      # Invalid
        [ Subscription_formlet.Invalid_age (-5)
        ; Subscription_formlet.Unchecked_rules ]
    ]}

    The positive point of this approach is that it decorrelates the creation
    function of the formlet and its validation functions. *)

(** {1 Type} *)

type ('a, 'errors) t =
  | Valid of 'a
  | Invalid of 'errors

(** {1 Implementation} *)

(** {2 Functor API} *)
module Functor (T : Preface_specs.Types.T0) :
  Preface_specs.FUNCTOR with type 'a t = ('a, T.t) t

(** {2 Applicative API} *)
module Applicative (Errors : Preface_specs.SEMIGROUP) :
  Preface_specs.APPLICATIVE with type 'a t = ('a, Errors.t) t

(** {2 Selective API} *)
module Selective (Errors : Preface_specs.SEMIGROUP) :
  Preface_specs.SELECTIVE
    with type 'a t = ('a, Errors.t) t
     and type ('a, 'b) either = ('a, 'b) Preface_core.Either.t

(** {2 Monad API} *)
module Monad (T : Preface_specs.Types.T0) :
  Preface_specs.MONAD with type 'a t = ('a, T.t) t

(** {1 Helpers} *)

val valid : 'a -> ('a, 'b) t
(** Wrap a value into [Valid].*)

val invalid : 'b -> ('a, 'b) t
(** Wrap an error value [Invalid]. *)

val pure : 'a -> ('a, 'b) t
(** Alias for [valid]. *)

val case : ('a -> 'c) -> ('b -> 'c) -> ('a, 'b) t -> 'c
(** [case f g x] apply [f] if [x] is [Valid], [g] if [x] is [Invalid].*)

val eq :
  ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
(** Equality. *)

val pp :
     (Format.formatter -> 'a -> unit)
  -> (Format.formatter -> 'b -> unit)
  -> Format.formatter
  -> ('a, 'b) t
  -> unit
(** Pretty printing. *)
