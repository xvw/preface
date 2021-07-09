```ocaml
# #require "preface"
# #install_printer Preface.Nonempty_list.pp
```

# Error management in a functional way

Error handling, in a functional style, is often very well
understood. However, Preface describes several modules for handling
errors. In this guide, we propose to show you how to take advantage of
them to functionally manage computations that may fail.

The standard Preface library exposes [several
modules](https://ocaml-preface.github.io/preface/Preface/index.html#error-handling)
to describe computations that can potentially fail:

- [Result](https://ocaml-preface.github.io/preface/Preface_stdlib/Result/index.html):
  a type describing **sequential error validations**.
  In a pipeline of failable computing, as soon as a function
  fails, the computation is interrupted.
- [Try](https://ocaml-preface.github.io/preface/Preface_stdlib/Try/index.html):
  As `Result` is parameterized by two types, `Try` is a specialised version
  of `Result`. Its error type is set to be an exception (`'a Try.t = ('a, exn) Result.t`).
- [Validation](https://ocaml-preface.github.io/preface/Preface_stdlib/Validation/index.html):
  a type describing **parallel error validations**. In a failable computing pipeline, all errors can be
  accumulated.
- [Validate](https://ocaml-preface.github.io/preface/Preface_stdlib/Validate/index.html):
  As `Validation` is parameterized by two types, `Validate` is a
  specialised version of `Validation`. Its error type is set to be a
  non-empty list of exception, because if there is an error, then at
  least one error has occurred. So it wouldn't make sense to have a
  normal list of errors. (`'a Validate.t = ('a, exn Nonempty_list.t) Validation.t`).

There is also
[Either](https://ocaml-preface.github.io/preface/Preface_stdlib/Either/index.html)
which is strictly equivalent to `Result` (`('a, 'b) Either.t = ('b, 'a) Result.t`). But its constructors carry less meaning, in the
context of error handling, so we will not use them in this guide.

As exceptions are an open sum type, they are comfortable for declaring
errors by necessity. However, to ensure completeness of error handling,
you might want to handle your own error list, hence the presence of
generalised versions of `Result` and `Validation`. To simplify the
content of this guide, we will only use the specialised versions (`Try` and
`Validate`) but bear in mind that you can specialise `Result` and
`Validation` yourself to describe your own error format. The only
constraint is that `Validation` errors must be
[`Semigroup`](https://ocaml-preface.github.io/preface/Preface_specs/Semigroup/index.html).

# Sequential validation

Sequential validation is the literal transformation of impure
functions (which may throw exceptions) into pure functions. We wrap
the result in `Ok` if it is valid or wrap the error in `Error` if it
is invalid. For example, let's implement a safe version of
`divide` invalidating the result if the divisor is
equal to `0`:

```ocaml
exception Division_by_zero

let safe_divide d x =
  if d = 0 then Error Division_by_zero
  else Ok (x / d)
```

Now we can use it without fear. Since `Try` is a monad, it is possible
to use `bind` and `map` to chain operations:

```ocaml
# let result =
    let open Preface.Try.Monad in
    return 42
       >>= safe_divide 2
       >|= succ
       >|= succ
       >>= safe_divide 2
    ;;
val result : int Preface.Try.Monad.t = Ok 11
```

We could also perform a computation that could fail... by dividing by
zero!

```ocaml
# let failed =
    let open Preface.Try.Monad in
    return 42
       >>= safe_divide 0
       >|= succ
       >|= succ
       >>= safe_divide 2
    ;;
val failed : int Preface.Try.Monad.t = Error Division_by_zero
```

And as with all monads, one can of course take advantage of
`let-operators` to describe programs that approach the direct style.
Here is the first example using the syntactic versions of `bind` and
`map`:

```ocaml
# let result =
    let open Preface.Try.Monad in
    let* w = return 42 in
    let* x = safe_divide 2 w in
    let y = succ x in
    let z = succ y in
    safe_divide 2 z
    ;;
val result : int Preface.Try.Monad.t = Ok 11
```

This pattern makes it possible to model quite complex error cases,
provided the validation is sequential. There is an extremely
pedagogical presentation by Scott Wlaschin on this approach, [Railway
oriented programming: Error handling in functional
languages](https://vimeo.com/113707214), and I suggest you watch it if
you want to see more scenarios.

## Parallel validation

But sometimes sequential validation can be a dark-pattern. For
example, let's imagine that we want to validate user
information before creating them in database:

```ocaml
type user = {
  nickname : string (* The nickname must have at least two chars. *)
; age: int (* should be a strictly positive integer, greater than 7. *)
; email : string (* should be a potentially valid email. *)
}

let make_user nickname age email = {nickname; age; email}
```

Imagine proceeding in a sequential manner:

```ocaml non-deterministic=command
check_nickname nickname >>= fun nickname ->
check_age age           >>= fun age ->
check_email email       >|= fun email ->
make_user nickname age email
```

Sequential validation would mean going back and forth between input
and validation! Ah, my nickname is too short, let's enlarge it! What,
my e-mail is not valid... **rah**. That can be really annoying! In
this example, we would like all errors to be collected, so that the
user knows right away what changes they need to make (for
example, finding a longer nickname and lying about age).

This approach is only possible because each field does not depend on
the others, so they can be validated independently, and for this we
will use the `Applicative` instance of `Validate`.

First, let's describe our validation functions:

```ocaml
exception Nickname_too_short of string
exception Invalid_email of string
exception Invalid_age of int

let check_nickname nickname =
  let open Preface.Validate in
  if String.length nickname >= 2 then valid nickname
  else error (Nickname_too_short nickname)

let check_age age =
  let open Preface.Validate in
  if age > 7 then valid age
  else error (Invalid_age age)

let check_email email =
  let open Preface.Validate in
  (* Yes, it is a pretty weak implementation for
     validating an email *)
  match String.split_on_char '@' email with
  | [_; _] -> valid email
  | _ -> error (Invalid_email email)
```

The `error` function (located in the module `Validate`) wraps an
exception in a non-empty list and wraps that non-empty list in the
`invalid` part of a validation (remember, `Validate` is just a skewed
version of `Validation`).

Now, using the `Validate` applicative combiners, we can apply parallel
validation:

```ocaml
let create_user nickname age email =
   let open Preface.Validate.Applicative in
   make_user
     <$> check_nickname nickname
     <*> check_age age
     <*> check_email email
```

Let's try our function with a valid user right away!

```ocaml
# create_user "xvw" 31 "xaviervdw@gmail.com" ;;
- : user Preface.Validate.Selective.t =
Preface_stdlib__.Validation.Valid
 {nickname = "xvw"; age = 31; email = "xaviervdw@gmail.com"}
```

Great, it works perfectly when our user is valid! Let's try it now
with an invalid email address:

```ocaml
# create_user "xvw" 31 "xaviervdwgmail.com" ;;
- : user Preface_stdlib.Validate.Selective.t =
Preface_stdlib__.Validation.Invalid [Invalid_email "xaviervdwgmail.com"]
```

Great! Now let's try it with ANYTHING that doesn't follow the rules!
(And yes, there are plenty of bad users on the net!)

```ocaml
# create_user "x" (-23) "abademail" ;;
- : user Preface_stdlib.Validate.Selective.t =
Preface_stdlib__.Validation.Invalid
 [Nickname_too_short "x"; Invalid_age (-23); Invalid_email "abademail"]
```

Perfect, all errors are well collected!

### How to handle the validation of a group of users?

Sometimes we would like to reuse the user validation function for a
list of users (or for any collection of users). For that, we would
like to go, for example from `(user Validate.t) list` to `(user list) Validate.t`. For this task, we can use the [`Traversable` module which
is in
List](https://ocaml-preface.github.io/preface/Preface_stdlib/List/Applicative/Traversable/index.html)!

First, let's build a module that allows **traversing** a list of
**validated users**:

```ocaml
module Validated_list =
   Preface.List.Applicative.Traversable
       (Preface.Validate.Applicative)
```

Now we can use, for example, the `sequence` function whose type is:
`'a Validate.t list -> 'a list Validate.t` which is exactly
what we were looking for:

```ocaml
# [ create_user "xvw" 31 "xaviervdw@gmail.com"
  ; create_user "yvw" 32 "yaviervdw@gmail.com"
  ; create_user "zvw" 33 "zaviervdw@gmail.com"
  ] |> Validated_list.sequence
- : user list Validated_list.t =
Preface_stdlib__.Validation.Valid
 [{nickname = "xvw"; age = 31; email = "xaviervdw@gmail.com"};
  {nickname = "yvw"; age = 32; email = "yaviervdw@gmail.com"};
  {nickname = "zvw"; age = 33; email = "zaviervdw@gmail.com"}]
```

Great we have a User List Validation and not a User Validation List
anymore! What if we create invalid users?

```ocaml
# [ create_user "xvw" 31 "xaviervdw@gmail.com"
  ; create_user "yvw" (-32) "yaviervdw@gmail.com"
  ; create_user "zvw" 33 "zaviervdwgmail.com"
  ] |> Validated_list.sequence
- : user list Validated_list.t =
Preface_stdlib__.Validation.Invalid
 [Invalid_age (-32); Invalid_email "zaviervdwgmail.com"]
```

Excellent, we have an invalid result with all the errors that occurred
in the list!

## A first conclusion

`Result` and `Validation` (or their specialised `Try` and` Validate`
versions) allow dealing with, respectively, **sequential validation**
and **parallel validation**. Naturally, sequential validation goes
better with monadic processing while parallel validation goes better
with applicative processing. And the responsibility is left to the
user to choose sequential or parallel validation or a mixture of both,

More general versions are still very useful because sometimes choosing
errors (such as exceptions or a non-empty list of exceptions) is not
enough to describe the errors correctly. For example, in our last
example, it is not possible to know exactly which are the invalid
users. All errors are at the same level.

## Let's go further, derive a canonical representation of validable data

We are able to validate (sequentially or in parallel) structured
data. Wouldn't it be nice to be able to derive an arbitrary (or
canonical) representation from a validation function, for example,
from an user-validation, deriving an HTML form (a **formlet**) for
filling a user? To sum up, we want:

- a DSL for describing arbitrary validation pipelines
- the ability to perform the validation described by the DSL
  function
- the ability to derive a representation to an arbitrary representation
  (ie: an HTML form)

This part of the guide is a fairly loose interpretation of the
technique described in [The Essence of Form
Abstraction](https://homepages.inf.ed.ac.uk/slindley/papers/formlets-essence.pdf)
by E. Cooper, S. Lindley, P. Wadler and J. Yallop.

First, we need something to characterise a form:

```ocaml
type form_type =
  | Password
  | Text of int option * int option
  | Checkbox
  | Integer of int option * int option
  | Email
type form_element = { label : string; form_type : form_type }
```

Concretely, for the needs of the guide, one is satisfied to describe a
form by its type and a label. Then, we need something to describe a
`validable field`. Either have a validation function and its
representation in form:

```ocaml
type 'a field = {
  validator : string -> 'a Preface.Validate.t
; repr : form_element
}
```

Now, we can define some combinators to describe validable fields.
First, let's define a validator that can validate a string is
indeed an integer with a potential bound:

```ocaml
exception Invalid_int of string
exception Int_greater of int * int
exception Int_lower of int * int

let validate_int ?min ?max str_int =
  let open Preface.Validate in
  match int_of_string_opt str_int with
  | None -> error (Invalid_int str_int)
  | Some x ->
      let min = Option.value ~default:min_int min
      and max = Option.value ~default:max_int max in
      if x < min then error (Int_lower (x, min))
      else if x > max then error (Int_greater (x, max))
      else valid x
```

Now, let's define a validator to ensure that a string is of a certain
size:

```ocaml
exception String_too_short of string * int
exception String_too_large of string * int

let validate_string ?min ?max str =
  let open Preface.Validate in
  let len = String.length str in
  let min = Option.value ~default:min_int min
  and max = Option.value ~default:max_int max in
  if len < min then error (String_too_short (str, min))
  else if len > max then error (String_too_large (str, max))
  else valid str
```

We can get our old (weak) email validation function:

```ocaml
let validate_email email =
  let open Preface.Validate in
  (* Yes, it is a pretty weak implementation for
     validating an email *)
  match String.split_on_char '@' email with
  | [_; _] -> valid email
  | _ -> error (Invalid_email email)
```

And now we just have to validate that a boolean is true :

```ocaml
exception Unchecked_bool

let validate_bool str =
  let open Preface.Validate in
  let f = str
    |> String.trim
    |> String.lowercase_ascii
  in match f with
  | "false" -> error Unchecked_bool
  | _ -> valid true
```

### Use of a Free Applicative

Now that we have a validator collection, it is necessary to build our
DSL. For this, we will use a [Free
Applicative](https://ocaml-preface.github.io/preface/Preface_make/Free_applicative/index.html). Like
all other free constructs, the idea behind a `Free Applicative` is to
provide an `Applicative` with a weaker construct: here a `Functor`. So
let's implement a `Functor` for our type `'a field`. The
implementation is trivial:

```ocaml
module Functor = Preface.Make.Functor.Via_map (struct
   type 'a t = 'a field
   let map f field = { field with
      validator = (fun x ->
         let open Preface.Validation in
         match field.validator x with
         | Invalid x -> Invalid x
         | Valid value -> Valid (f value)
      )
   }
end)
```

Now that we have a Functor, we can build an instance of `Applicative`,
**for free**:

```ocaml
module Formlet = Preface.Make.Free_applicative.Over_functor (Functor)
```

Once our `Applicative` is built, we have a `promote` function that transforms a value of `'a field` into `'a Formlet.t` (a value wrapped in our `Free Applicative`). So let's define combinators using our previous validators:

```ocaml
let int ?min ?max name =
  Formlet.promote {
    validator = validate_int ?min ?max
  ; repr = { label = name; form_type = Integer (min,  max)}
  }

let password =
  Formlet.promote {
    validator = validate_string ~min:5 ?max:None
  ; repr = { label = "password"; form_type = Password}
  }

let string ?min ?max name =
  Formlet.promote {
    validator = validate_string ?min ?max
  ; repr = { label = name; form_type = Text (min,  max)}
  }

let email =
  Formlet.promote {
    validator = validate_email
  ; repr = { label = "email"; form_type = Email}
  }

let rules_checked =
  Formlet.promote {
    validator = validate_bool
  ; repr = { label = "checked_rules"; form_type = Checkbox}
  }
```

Now that all our combinators are defined, we have a DSL. This can be
used to validate arbitrary data structures. For example, let's imagine
a form allowing a user to register on our site!

```ocaml
module Registration = struct

  type t = {
    name : string
  ; nickname : string
  ; age : int
  ; email : string
  ; password : string
  ; checked_rules : bool
  }

  let make name nickname age email password checked_rules =
    { name; nickname; age; email; password; checked_rules }

  (* And we can define validation over registration *)
  let validate =
    (* Formlet is a [Free Applicative],
       so it also an [Applicative]. *)
    let open Formlet in
    make
      <$> string ~min:2 "name"
      <*> string ~min:2 ~max:25 "nickname"
      <*> int ~min:7 ~max:99 "age" (* Discrimination for the elders! *)
      <*> email
      <*> password
      <*> rules_checked
end
```

Now we assume that the data to be validated comes from an HTTP
request, the HTTP variables could be stored in a simple `"key" => "values"` association list:

```ocaml
exception Missing_field of string
let read_http_variables http_variables field =
  let label = field.repr.label in
  match List.assoc_opt label http_variables with
  | None -> Preface.Validate.error (Missing_field label)
  | Some value -> field.validator value
```

### Concretely validate data

Now we know how to look for our data and how to validate it. We need
to provide a transformation from our `Formlet` to a `Validate`. The
construction of a `Free Applicative` exposes a `To_applicative` module
which is a **natural transformation** of the `Free Applicative` to
another `Applicative`:

```ocaml
module Run = Formlet.To_applicative (Preface.Validate.Applicative)
```

Now we can define a function whose role will be to use our function
that (pretends) to read HTTP variables:

```ocaml
let run_registration http_variables =
  let open Run in
  Run.run
    { transform = fun key ->
        read_http_variables http_variables key
    } Registration.validate
```

Now we can simulate some examples. Let's imagine that we haven't
filled in any values in the form:

```ocaml
# run_registration [];;
- : Registration.t Validated_list.t =
Preface_stdlib__.Validation.Invalid
 [Missing_field "checked_rules"; Missing_field "password";
 Missing_field "email"; Missing_field "age"; Missing_field "nickname";
 Missing_field "name"]
```

Everything seems to work, the function replies that all fields are missing!
Now let's try a slightly trickier
example. Let's only fill in the email address incorrectly:

```ocaml
# run_registration [
    ("name", "Xavier Van de Woestyne")
  ; ("nickname", "xvw")
  ; ("age", "31")
  ; ("email", "a bad email")
  ; ("password", "a-supposed-valid-password")
  ; ("checked_rules", "true")
  ];;
- : Registration.t Validated_list.t =
Preface_stdlib__.Validation.Invalid [Invalid_email "a bad email"]
```

Yes, the error is correctly reported! Let's try one last case to make sure everything is OK in the unhappy path!

```ocaml
# run_registration [
    ("name", "X")
  ; ("nickname", "xvw")
  ; ("age", "4")
  ; ("email", "xvw@gmail.com")
  ; ("password", "a-supposed-valid-password")
  ; ("checked_rules", "false")
  ];;
- : Registration.t Validated_list.t =
Preface_stdlib__.Validation.Invalid
 [Unchecked_bool; Int_lower (4, 7); String_too_short ("X", 2)]
```

Great, everything seems to work when you make mistakes. Now let's try
to create a valid registration!

```ocaml
# run_registration [
    ("name", "Xavier Van de Woestyne")
  ; ("nickname", "xvw")
  ; ("age", "31")
  ; ("email", "xaviervdw@gmail.com")
  ; ("password", "a-supposed-valid-password")
  ; ("checked_rules", "true")
  ];;
- : Registration.t Validated_list.t =
Preface_stdlib__.Validation.Valid
 {Registration.name = "Xavier Van de Woestyne"; nickname = "xvw"; age = 31;
  email = "xaviervdw@gmail.com"; password = "a-supposed-valid-password";
  checked_rules = true}
```

Awesome, it works!

But then... at this point, you're probably wondering what the point of
all this is? Even if it seems that it is possible to combine
validation scenarios more freely... we don't seem to be any further
ahead than before. Because, let's face it, it would have been possible
to provide our validation functions with the environment to observe,
for example:

```ocaml
let validate_int' env name =
   let open Preface.Validate in
   match List.assoc_opt name env with
   | None -> error (Missing_field name)
   | Some x ->
      int_of_string_opt x
      |> Option.fold ~some:valid ~none:(error (Invalid_int x))
```

Which would have given us exactly the same effect, so why bother with
a `Free Applicative`? Well, because in addition to `To_applicative`, a
`Free Applicative` offers another internal module: `To_monoid`! At
the beginning of this section I boasted that it was possible not only
to describe a validation pipeline by means of a DSL, but also to
derive a **canonical representation** from a program described in that
DSL, ie: an HTML representation. Well, as you can see, it is possible
to use the `repr` field in our validator to represent an HTML
representation of our registration validator, we are going to declare
a module that will collect all the representations of our fields in a
list. As we know the type of our elements: `form_element`, this is a
monomorphic list, so it can be considered as a monoid:

```ocaml
module Repr = Formlet.To_monoid (Preface.List.Monoid (struct
  type t = form_element
end))
```

And just like the projection in the `Applicative` `Validate`, we will
be able to capture all the representations of our fields by means of a
**natural transformation**:

```ocaml
let registration_representation =
  let open Repr in
  Repr.run
    { transform = fun field -> [ field.repr ] }
    Registration.validate
```

Now that we have collected all our fields, we can **inspect** them:

```ocaml
# registration_representation ;;
- : form_element list =
[{label = "checked_rules"; form_type = Checkbox};
 {label = "password"; form_type = Password};
 {label = "email"; form_type = Email};
 {label = "age"; form_type = Integer (Some 7, Some 99)};
 {label = "nickname"; form_type = Text (Some 2, Some 25)};
 {label = "name"; form_type = Text (Some 2, None)}]
```

We can perform a static observation of the fields to be validated. So
it is perfectly possible to generate the HTML representation. To do
this properly we would need to use
[TyXML](https://ocsigen.org/tyxml/latest/manual/intro), however for
the purposes of this tutorial we will simply use strings and we don't
handle every case (ie, min and max length in order to keep the code
readable):

```ocaml
let node_to_html name = let open Format in  function
 | Checkbox ->
     asprintf
        {|<input type="checkbox" name="%s">|}
        name
 | Password ->
      asprintf
        {|<input type="password" name="%s" minlength="8" required>|}
        name
 | Text (_, _) ->
      asprintf
        {|<input type="text" name="%s">|}
        name
 | Email  ->
      asprintf
        {|<input type="email" name="%s">|}
        name
 | Integer (_, _) ->
       asprintf
        {|<input type="number" name="%s">|}
        name

let repr_to_html {label; form_type} =
   Format.asprintf
        "<div>\n  <span>%s:</span>\n  %s\n</div>"
        label
        (node_to_html label form_type)

let html_form =
   registration_representation
   |> List.rev
   |> List.map repr_to_html
   |> String.concat "\n"
```

After this quick and dirty encoding of our form, we can just display
it, and voila, a form derived from the `Registration.validate`
description:

```ocaml
# print_endline (html_form) ;;
<div>
  <span>name:</span>
  <input type="text" name="name">
</div>
<div>
  <span>nickname:</span>
  <input type="text" name="nickname">
</div>
<div>
  <span>age:</span>
  <input type="number" name="age">
</div>
<div>
  <span>email:</span>
  <input type="email" name="email">
</div>
<div>
  <span>password:</span>
  <input type="password" name="password" minlength="8" required>
</div>
<div>
  <span>checked_rules:</span>
  <input type="checkbox" name="checked_rules">
</div>
- : unit = ()
```

## Conclusion

Having seen how to validate data sequentially and then in parallel, we
turned to a slightly less conventional example of applicative
validation. The use of a `Free Applicative` allows to describe an
**arbitrary DSL** to validate data coming from a source and to exploit
this DSL to produce a representation (here, an HTML
form). [Cmdliner](https://github.com/dbuenzli/cmdliner) is an
excellent use of this encoding. It is used to describe the entry point
of a CLI and, in addition to generating the function that boots the
CLI, it is used to describe the MAN pages of the CLI being described.
