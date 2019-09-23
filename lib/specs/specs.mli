(** The [Specs] module handle all signatures related to [Preface].
    When any object is expressed in [Preface], its interfaces are often
    expressed in this module.

    The modules below are generally wrappers for several types of submodules
    (in the sense of [module type of ...]). They strive to follow respect
    these conventions:

    - [Obj.API]: describes the complete interface of the object;
    - [Obj.REQUIREMENT]: describes an interface for the requirements of the
      parameter of a parametrized module, to produce the object.

    In general, the various parameterized modules of the library follow this
    type:

    {[module Make_Obj(M : Obj.REQUIREMENTS) : Obj.API]}

    Sometimes it happens that an object has several requirements.
    For example, if there are several ways to build the interface of this
    object (for example, the production of a module for a [Monad] is possible
    via at least 3 approaches). And [Functor], for example, does not handle
    any requirement.
*)

(** {1 Full Interfaces}
    Each objects, packed with requirements and API.
*)

module Functor = Functor
(** Describes an object that can be mapped. *)

module Applicative = Applicative
(** Describes an applicative functor *)

(** {1 API Shortcuts}
    Shortcuts for the API of each objects (by convention, OCaml module types
    are in uppercase.
*)

module type FUNCTOR = Functor.API

module type APPLICATIVE = Applicative.API
