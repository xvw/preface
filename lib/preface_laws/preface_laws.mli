(** The abstractions described in [Preface] generally impose laws (or behaviour)
    that must be respected to ensure that the derived combinators have the
    expected behaviour. This library allows the generation of structured laws
    ({!module:Law}) for a concrete representation of an abstraction. They are
    notably used in tests.

    Each module that produces laws prefixes the name of the law with the
    abstraction to avoid collisions between certain names. For example
    [Applicative] also embeds the laws of [Functors]. *)

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
module Indexed_applicative = Indexed_applicative
module Indexed_alternative = Indexed_alternative
module Indexed_selective = Indexed_selective
module Indexed_bind = Indexed_bind
module Indexed_monad = Indexed_monad
module Indexed_monad_plus = Indexed_monad_plus
module Indexed_comonad = Indexed_comonad

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

(** {1 Descriptions of laws}

    Describes a law as structured data. *)

module Law = Law
