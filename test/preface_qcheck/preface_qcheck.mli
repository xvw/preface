(** A QCheck (https://github.com/c-cube/qcheck) companion for Preface
    development. *)

module Exceptions = Exceptions
(** {2 Generate exceptions} *)

(** {1 QCheck tools} *)

module Gen = Gen
(** {2 Generate Random Values} *)

module Shrink = Shrink
(** {2 Reduce values} *)

module Arbitrary = Arbitrary
(** {2 Generator/Printing/Shrinking} *)

module Sample = Sample
(** {2 Sample values} *)

(** {1 Laws and behaviours generators} *)

module Requirement = Requirement
module Functor = Functor
module Applicative = Applicative
module Monad = Monad
module Bifunctor = Bifunctor
module Selective = Selective
module Semigroup = Semigroup
module Monoid = Monoid
