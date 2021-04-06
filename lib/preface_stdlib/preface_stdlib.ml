(** {2 Identity}

    Identity is simply described as [type 'a t = 'a]. And is quite handy when
    functions/functors need to act on monads (e.g. [State], [Writer] and
    [Reader]), it allows to turn any type, arbitrarily, into a monad. *)

module Identity = Identity

(** {2 Common datatypes} *)

module Option = Option
module Either = Either
module Pair = Pair

(** {2 Collection} *)

module List = List
module Nonempty_list = Nonempty_list
module Stream = Stream

(** {2 Error handling} *)

module Result = Result
module Validation = Validation
module Try = Try
module Validate = Validate
module Exn = Exn

(** {2 Functions} *)

module Fun = Fun
module Predicate = Predicate
module Continuation = Continuation

(** {2 Common effects}

    [State], [Writer] and [Reader] are defined in [Specs/Make] as monad
    transformers. In [Stdlib] these are concretised using [Identity] as inner
    monad. *)

module Reader = Reader
module Writer = Writer
module State = State

(** {2 Static Analysis}

    [Applicatives], [Selectives], [Profunctors] and [Arrows] allow, contrary to
    monads, to perform static analyses on calculation workflows. [Over] and
    [Under] allow optimistic or pessimistic approximations. *)

module Approximation = Approximation
