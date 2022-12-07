(** Some pre-registered requirements. *)

(** {1 Types of arity 0}

    Describes the set of prerequisites for building generators on arity-free
    types (notably the monoid hierarchy). *)

module Exn : Preface.Qcheck.Model.COVARIANT_0 with type t = Preface.Exn.t

(** {1 Types of arity 1} *)

module Identity :
  Preface.Qcheck.Model.COVARIANT_1 with type 'a t = 'a Preface.Identity.t

module Over
    (M : Preface.Specs.MONOID)
    (T : Preface.Qcheck.Model.T0 with type t = M.t) : sig
  include module type of Preface.Approximation.Over (M)
  module Req : Preface.Qcheck.Model.COVARIANT_1 with type 'a t = 'a t
end

module Under
    (M : Preface.Specs.MONOID)
    (T : Preface.Qcheck.Model.T0 with type t = M.t) : sig
  include module type of Preface.Approximation.Under (M)
  module Req : Preface.Qcheck.Model.COVARIANT_1 with type 'a t = 'a t
end

module List : sig
  include Preface.Qcheck.Model.COVARIANT_1 with type 'a t = 'a list

  module Mono (T : Preface.Qcheck.Model.T0) :
    Preface.Qcheck.Model.COVARIANT_0 with type t = T.t list
end

module Nonempty_list : sig
  include
    Preface.Qcheck.Model.COVARIANT_1 with type 'a t = 'a Preface.Nonempty_list.t

  module Mono (T : Preface.Qcheck.Model.T0) :
    Preface.Qcheck.Model.COVARIANT_0 with type t = T.t Preface.Nonempty_list.t
end

module Seq : sig
  include Preface.Qcheck.Model.COVARIANT_1 with type 'a t = 'a Stdlib.Seq.t

  module Mono (T : Preface.Qcheck.Model.T0) :
    Preface.Qcheck.Model.COVARIANT_0 with type t = T.t Stdlib.Seq.t
end

module Option : Preface.Qcheck.Model.COVARIANT_1 with type 'a t = 'a option
module Try : Preface.Qcheck.Model.COVARIANT_1 with type 'a t = 'a Preface.Try.t

module Validate :
  Preface.Qcheck.Model.COVARIANT_1 with type 'a t = 'a Preface.Validate.t

module Stream :
  Preface.Qcheck.Model.COVARIANT_1 with type 'a t = 'a Preface.Stream.t

module Continuation :
  Preface.Qcheck.Model.COVARIANT_1 with type 'a t = 'a Preface.Continuation.t

(** {1 Types of arity 2} *)

module Result : sig
  include
    Preface.Qcheck.Model.COVARIANT_2
      with type ('a, 'b) t = ('a, 'b) Preface.Result.t

  module Mono (T : Preface.Qcheck.Model.T0) :
    Preface.Qcheck.Model.COVARIANT_1 with type 'a t = ('a, T.t) Preface.Result.t
end

module Validation : sig
  include
    Preface.Qcheck.Model.COVARIANT_2
      with type ('a, 'b) t = ('a, 'b) Preface.Validation.t

  module Mono (T : Preface.Qcheck.Model.T0) :
    Preface.Qcheck.Model.COVARIANT_1
      with type 'a t = ('a, T.t) Preface.Validation.t
end

module Either : sig
  include
    Preface.Qcheck.Model.COVARIANT_2
      with type ('a, 'b) t = ('a, 'b) Preface.Either.t

  module Mono (T : Preface.Qcheck.Model.T0) :
    Preface.Qcheck.Model.COVARIANT_1 with type 'a t = (T.t, 'a) Preface.Either.t
end

module Pair :
  Preface.Qcheck.Model.COVARIANT_2
    with type ('a, 'b) t = ('a, 'b) Preface.Pair.t

module Predicate :
  Preface.Qcheck.Model.CONTRAVARIANT_1 with type 'a t = 'a Preface.Predicate.t

module Equivalence :
  Preface.Qcheck.Model.CONTRAVARIANT_1 with type 'a t = 'a Preface.Equivalence.t

module Fun :
  Preface.Qcheck.Model.PROFUNCTORIAL
    with type ('a, 'b) t = ('a, 'b) Preface.Fun.t

(** {1 Misc examples} *)

module Mini_yocaml : sig
  type ('a, 'b) t

  module Req :
    Preface.Qcheck.Model.PROFUNCTORIAL with type ('a, 'b) t = ('a, 'b) t

  module Category : Preface.Specs.CATEGORY with type ('a, 'b) t = ('a, 'b) t
  module Profunctor : Preface.Specs.PROFUNCTOR with type ('a, 'b) t = ('a, 'b) t
  module Strong : Preface.Specs.STRONG with type ('a, 'b) t = ('a, 'b) t
  module Choice : Preface.Specs.CHOICE with type ('a, 'b) t = ('a, 'b) t
  module Arrow : Preface.Specs.ARROW with type ('a, 'b) t = ('a, 'b) t

  module Arrow_choice :
    Preface.Specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) t
end
