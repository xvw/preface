(** Some pre-registered requirements. *)

module Identity :
  Preface.Qcheck.Model.COVARIANT_1 with type 'a t = 'a Preface.Identity.t

module List : Preface.Qcheck.Model.COVARIANT_1 with type 'a t = 'a list
module Option : Preface.Qcheck.Model.COVARIANT_1 with type 'a t = 'a option
module Try : Preface.Qcheck.Model.COVARIANT_1 with type 'a t = 'a Preface.Try.t

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
