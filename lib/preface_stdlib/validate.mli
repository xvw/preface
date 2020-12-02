(** Exposes [Validate.t]; a biaised version of [Validation.t] with the errors
    fixed as a non empty list of exceptions.

    - {!val:Functor}
    - {!val:Applicative}
    - {!val:Selective}
    - {!val:Monad}

    {1 Use cases}

    [Validate] allows you to create validation pipelines that accumulate errors.
    Unlike [Try], which performs sequential validations, [Validate] goes well
    with the applicative composition.

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
          if age >= 0
          then Validate.valid age
          else Validate.error (Invalid_age age)
        ;;

        let validate_name subject name =
          if String.length name > 1
          then Validate.valid name
          else Validate.error (Invalid_name (subject, name))
        ;;

        let validate_rules checked =
          if checked
          then Validate.valid checked
          else Validate.error Unchecked_rules
        ;;
      end
    ]}

    We can now use our [make] functions and our validation functions to validate
    the creation of a subscription:

    {[
      let try_register age firstname lastname checked_rules =
        let open Subscription_formlet in
        let open Validate.Applicative.Infix in
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

(** {1 Imported function} *)

include Preface_specs.Package.APPLICATIVE_AND_MONAD with type 'a t := 'a t

(** {1 Helpers} *)

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

val eq : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Equality.*)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** Pretty printing. *)
