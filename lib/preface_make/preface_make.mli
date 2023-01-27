(** Set of functors ({e in ML sense}) whose role is {e to achieve} the
    abstractions described in {!module:Preface_specs}. Each abstraction
    described in the specifications has its image in [Preface.Make] and the
    functors take as arguments modules constrained by the signatures described
    in {!module:Preface_specs} and produce modules whose complete interface is
    also described in {!module:Preface_specs}.)

    For a detailed description of the module breakdown logic,
    {{:../Preface/index.html#concepts,-naming-and-terminology} go to the
    homepage}.

    {2 Multiple path}

    The achievement of an abstraction usually offers several paths:

    - using the minimal definition or implement as few combiners as possible.
      Some abstractions offer several minimal definitions, so it is up to you to
      choose the path that seems most relevant .
    - A construction on top of another abstraction (ie:
      {!module-type:Preface_specs.MONOID} defined over a
      {!module-type:Preface_specs.SEMIGROUP})
    - A manual approach where each feature has to be provided (while offering
      intermediate functors to avoid boring tasks such as defining infix
      operators).
    - A simplification of an existing abstraction, such as turning a
      {!module-type:Preface_specs.ALTERNATIVE} into a
      {!module-type:Preface_specs.MONOID} by fixing the type of the Alternative.
    - Using a fixed algebra. For example, the sum, product or composition of
      {!module-type:Preface_specs.FUNCTOR}.*)

(** {1 Monoid hierarchy} *)

module Semigroup = Semigroup
module Monoid = Monoid

(** {1 Lattice hierarchy} *)

module Meet_semilattice = Meet_semilattice
module Join_semilattice = Join_semilattice
module Bounded_meet_semilattice = Bounded_meet_semilattice
module Bounded_join_semilattice = Bounded_join_semilattice

(** {1 Indexed Functor hierarchy} *)

module Indexed_functor = Indexed_functor
module Indexed_alt = Indexed_alt
module Indexed_apply = Indexed_apply

(** {1 Functor hierarchy} *)

module Invariant = Invariant
module Functor = Functor
module Alt = Alt
module Apply = Apply
module Applicative = Applicative
module Alternative = Alternative
module Selective = Selective
module Bind = Bind
module Monad = Monad
module Monad_plus = Monad_plus
module Comonad = Comonad
module Foldable = Foldable
module Traversable = Traversable

(** {1 Contravariant hierarchy} *)

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

(** {1 Conversion and Expansion}

    Produces abstractions based on other abstractions (e.g. bifunctors from
    functors). Mainly for dealing with different arities. *)

module Join = Join
module Clown = Clown
module Joker = Joker
module Kleisli = Kleisli
module Cokleisli = Cokleisli
