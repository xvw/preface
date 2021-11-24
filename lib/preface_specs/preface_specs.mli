(** Set of interfaces describing all the abstractions available in Preface. For
    a detailed description of the module breakdown logic,
    {{:../Preface/index.html#concepts,-naming-and-terminology} go to the
    homepage}.

    {%html:
      <center>
        <a href="https://ocaml-preface.github.io/images/specs.svg"><img
          style="width:90%; margin: 10% 5%;"
          src="https://ocaml-preface.github.io/images/specs.svg"
          alt="Abstraction hierarchy"
        ></a>
      </center>%} *)

(** {1 Monoid hierarchy} *)

module Semigroup = Semigroup
module Monoid = Monoid

(** {1 Functor hierarchy} *)

module Invariant = Invariant
module Functor = Functor
module Alt = Alt
module Applicative = Applicative
module Alternative = Alternative
module Selective = Selective
module Monad = Monad
module Monad_plus = Monad_plus
module Comonad = Comonad
module Foldable = Foldable
module Traversable = Traversable

(** {1 Contravariant functor hierarchy} *)

module Contravariant = Contravariant
module Divisible = Divisible
module Decidable = Decidable

(** {1 Bifunctor hierarchy} *)

module Bifunctor = Bifunctor

(** {1 Profunctor hierarchy} *)

module Profunctor = Profunctor
module Strong = Strong
module Choice = Choice
module Closed = Closed

(** {1 Arrow hierarchy} *)

module Semigroupoid = Semigroupoid
module Category = Category
module Arrow = Arrow
module Arrow_zero = Arrow_zero
module Arrow_alt = Arrow_alt
module Arrow_plus = Arrow_plus
module Arrow_choice = Arrow_choice
module Arrow_apply = Arrow_apply

(** {1 Transformers} *)

(** {2 Monad Transformers} *)

module Reader = Reader
module Writer = Writer
module State = State

(** {2 Comonad Transformers} *)

module Store = Store
module Env = Env
module Traced = Traced

(** {1 Free constructions} *)

module Free_applicative = Free_applicative
module Free_selective = Free_selective
module Freer_selective = Freer_selective
module Free_monad = Free_monad
module Freer_monad = Freer_monad

(** {1 API Shortcuts}

    As each module exposes all the components of an abstraction, here is a list
    of shortcuts to directly denote the full API of an abstraction by using its
    name in upper case. *)

module type SEMIGROUP = Semigroup.API

module type MONOID = Monoid.API

module type FUNCTOR = Functor.API

module type BIFUNCTOR = Bifunctor.API

module type PROFUNCTOR = Profunctor.API

module type STRONG = Strong.API

module type CHOICE = Choice.API

module type CLOSED = Closed.API

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

module type FREER_SELECTIVE = Freer_selective.API

module type FREE_MONAD = Free_monad.API

module type FREER_MONAD = Freer_monad.API

module type INVARIANT = Invariant.API

module type CONTRAVARIANT = Contravariant.API

module type DIVISIBLE = Divisible.API

module type SEMIGROUPOID = Semigroupoid.API

module type DECIDABLE = Decidable.API

module type CATEGORY = Category.API

module type ARROW = Arrow.API

module type ARROW_ZERO = Arrow_zero.API

module type ARROW_ALT = Arrow_alt.API

module type ARROW_PLUS = Arrow_plus.API

module type ARROW_CHOICE = Arrow_choice.API

module type ARROW_APPLY = Arrow_apply.API

module type READER = Reader.API

module type WRITER = Writer.API

module type STATE = State.API

module type STORE = Store.API

module type ENV = Env.API

module type TRACED = Traced.API

(** {1 Types}

    Sometimes, in order to construct an abstraction, it is only necessary to
    give it a type (For example, {!module:Freer_monad}). *)

module Types = Types
