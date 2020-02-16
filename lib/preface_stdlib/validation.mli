(** Exposes [Validation.t]

    {1 Capabilities}

    - {!val:Functor}
    - {!val:Applicative}
    - {!val:Monad}

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
          if age >= 0 then Ok age else Error [ Invalid_age age ]
        ;;

        let validate_name subject name =
          if String.length name > 1
          then Ok name
          else Error [ Invalid_name (subject, name) ]
        ;;

        let validate_rules checked =
          if checked then Ok checked else Error [ Unchecked_rules ]
        ;;
      end
    ]}

    We can now use our [make] functions and our validation functions to validate
    the creation of a subscription:

    {[
      let try_register age firstname lastname checked_rules =
        let open Subscription_formlet in
        let open Preface.Validation.Applicative.Infix in
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
      # Ok
        {Subscription_formlet.age = 10; firstname = "Xavier";
         lastname = "Van de Woestyne"; checked_rules = true}
    ]}

    When the checkbox "Accept all the rules" is not checked!

    {[
      try_register 10 "Xavier" "Van de Woestyne" false;;
      # Error [Subscription_formlet.Unchecked_rules]
    ]}

    When the checkbox "Accept all the rules" is not checked and the [age] is
    negative!

    {[
      try_register (-5) "Xavier" "Van de Woestyne" false;;
      # Error
        [ Subscription_formlet.Invalid_age (-5)
        ; Subscription_formlet.Unchecked_rules ]
    ]}

    The positive point of this approach is that it decorrelates the creation
    function of the formlet and its validation functions. *)

(** {1 Type} *)

type 'a t = ('a, exn list) result

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

val error : exn list -> 'a t
(** Wrap an exception into [Error]. *)

val capture : (unit -> 'a) -> 'a t
(** [capture f] perform [f] and wrap the result into a [t] if the execution of
    [f] raise no exception, the result will be [Ok result] else, the catched
    exception if wrapped into [Error exn]. *)

val case : ('a -> 'b) -> (exn list -> 'b) -> 'a t -> 'b
(** [case f g x] apply [f] if [x] is [Ok], [g] if [x] is [Error].*)

val eq : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Equality.*)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** Pretty printing. *)
