(** The [Preface_specs] module handle all signatures related to [Preface]. When
    any object is expressed in [Preface], its interfaces are often expressed in
    this module.

    The modules below are generally wrappers for several types of submodules (in
    the sense of [module type of ...]). They strive to follow respect these
    conventions:

    - [Obj.API]: describes the complete interface of the object;
    - [Obj.REQUIREMENT]: describes an interface for the requirements of the
      parameter of a parametrized module, to produce the object.

    In general, the various parameterized modules of the library follow this
    type:

    {[ module Make_Obj(M : Obj.REQUIREMENTS) : Obj.API ]}

    Sometimes it happens that an object has several requirements. For example,
    if there are several ways to build the interface of this object (for
    example, the production of a module for a [Monad] is possible via at least 3
    approaches). And [Functor], for example, does not handle any requirement.

    {3 Conventions}

    To be able to let the user overload the operators of his choice, the
    different modules are usually divided into several components:

    - [Obj.CORE] : describes the basic operations of the object;
    - [Obj.OPERATION] : describes the additional operations (dependent on
      [CORE]) of the object;
    - [Obj.INFIX] : Describes the infix operators of the object (dependent on
      [CORE] and [OPERATION]);
    - [Obj.SYNTAX] : Describes the syntactic operators ([let+], [let*] and
      [and+] for example) of the object (dependencies vary depending on the
      object).

    Generally, the core and operations are included, where the syntax and the
    infix operators are, in addition to being included, exposed in a special
    sub-module to allow this kind of construction:

    {[ My_monad.Infix.(My_monad.return y >>= operation1 >>= operation2) ]} *)

(** {1 Misc} *)

module Requirements = Requirements

(** {1 Full Interfaces}

    Each objects, packed with requirements and API. *)

module Functor = Functor
(** Describes an object that can be mapped. *)

module Applicative = Applicative
(** Describes an applicative functor. *)

module Selective = Selective
(** Describes a selective applicative functor. *)

module Monad = Monad
(** Describes a monad. *)

module Comonad = Comonad
(** Describes a comonad. *)

module Traversable = Traversable
(** Describes a traversable. *)

module Free = Free
(** Describe a free monade. *)

module FreeEr = FreeEr
(** Describe a free monade. *)

(** {1 API Shortcuts}

    Shortcuts for the API of each objects (by convention, OCaml module types are
    in uppercase). *)

module type FUNCTOR = Functor.API

module type APPLICATIVE = Applicative.API

module type SELECTIVE = Selective.API

module type MONAD = Monad.API

module type COMONAD = Comonad.API

module type TRAVERSABLE = Traversable.API

module type FREE = Free.API

module type FREEER = FreeEr.API

(** {1 Abstract data types} *)

module type EITHER = Requirements.EITHER
(** [EITHER] hold a types which represents values with two possibilites. *)
