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

module Types = Types

(** {1 Full Interfaces}

    Each objects, packed with requirements and API. *)

module Semigroup = Semigroup
(** Describes an object than can be combined. *)

module Monoid = Monoid
(** Describes an object that can be combined and with a neutral element. *)

module Functor = Functor
(** Describes an object that can be mapped. *)

module Bifunctor = Bifunctor
(** Describes an object of two covariant functors. *)

module Profunctor = Profunctor
(** Describes and object where the first parameter is a contravariant functor
    and the second is a covariant functor.*)

module Strong = Strong
(** Describes a profunctor which can act on product/tuples. *)

module Choice = Choice
(** Describes a profunctor which can act on sum/Either. *)

module Applicative = Applicative
(** Describes an applicative functor. *)

module Alt = Alt
(** Describes an Alt. *)

module Alternative = Alternative
(** Describes an Alternative. *)

module Selective = Selective
(** Describes a selective applicative functor. *)

module Monad = Monad
(** Describes a monad. *)

module Monad_plus = Monad_plus
(** Describes a monad plus. *)

module Comonad = Comonad
(** Describes a comonad. *)

module Foldable = Foldable
(** Describes a foldable. *)

module Traversable = Traversable
(** Describes a traversable. *)

module Free_applicative = Free_applicative
(** Describes a free applicative. *)

module Free_selective = Free_selective
(** Describes a free selective. *)

module Free_monad = Free_monad
(** Describes a free monad. *)

module Freer_monad = Freer_monad
(** Describes a freer monad. *)

module Contravariant = Contravariant
(** Describes a Contravariant Functor. *)

module Category = Category
(** Describes a category. *)

module Arrow = Arrow
(** Describes an arrow. *)

module Arrow_zero = Arrow_zero
(** Describes an arrow zero. *)

module Arrow_alt = Arrow_alt
(** Describes an arrow alt. *)

module Arrow_plus = Arrow_plus
(** Describes an arrow plus. *)

module Arrow_choice = Arrow_choice
(** Describes an arrow choice. *)

module Arrow_apply = Arrow_apply
(** Describes an arrow apply. *)

module Arrow_loop = Arrow_loop
(** Describes an arrow loop. *)

(** {1 API Shortcuts}

    Shortcuts for the API of each objects (by convention, OCaml module types are
    in uppercase). *)

module type SEMIGROUP = Semigroup.API

module type MONOID = Monoid.API

module type FUNCTOR = Functor.API

module type BIFUNCTOR = Bifunctor.API

module type PROFUNCTOR = Profunctor.API

module type STRONG = Strong.API

module type CHOICE = Choice.API

module type APPLICATIVE = Applicative.API

module type ALT = Alt.API

module type ALTERNATIVE = Alternative.API

module type SELECTIVE = Selective.API

module type MONAD = Monad.API

module type MONAD_PLUS = Monad_plus.API

module type COMONAD = Comonad.API

module type FOLDABLE = Foldable.API

module type TRAVERSABLE = Traversable.API

module type FREE_APPLICATIVE = Free_applicative.API

module type FREE_SELECTIVE = Free_selective.API

module type FREE_MONAD = Free_monad.API

module type FREER_MONAD = Freer_monad.API

module type CONTRAVARIANT = Contravariant.API

module type CATEGORY = Category.API

module type ARROW = Arrow.API

module type ARROW_ZERO = Arrow_zero.API

module type ARROW_ALT = Arrow_alt.API

module type ARROW_PLUS = Arrow_plus.API

module type ARROW_CHOICE = Arrow_choice.API

module type ARROW_APPLY = Arrow_apply.API

module type ARROW_LOOP = Arrow_loop.API
