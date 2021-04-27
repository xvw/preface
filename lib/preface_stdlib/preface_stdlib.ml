(** *)
(* Empty comment because, currently, synospsis are not properly 
   handled when a module is included in another one. 
*)

(** {2 Common datatypes} *)

module Identity = Identity
module Option = Option
module Either = Either
module Pair = Pair

(** {2 Collection} *)

module List = List
module Nonempty_list = Nonempty_list
module Stream = Stream

(** {2 Error handling} *)

module Exn = Exn
module Result = Result
module Validation = Validation
module Try = Try
module Validate = Validate

(** {2 Functions} *)

module Fun = Fun
module Predicate = Predicate
module Continuation = Continuation

(** {2 Transformers over identity}

    There are some (monad or comonad) transformers defined in [Spec/Make]. In
    [Stdlib] these are some concretised version using [Identity] as inner monad
    or comonad. *)

module Reader = Reader
module Writer = Writer
module State = State
module Store = Store

(** {2 Static Analysis}

    [Applicatives], [Selectives], [Profunctors] and [Arrows] allow, contrary to
    monads, to perform static analyses on calculation workflows. [Over] and
    [Under] allow optimistic or pessimistic approximations. *)

module Approximation = Approximation
