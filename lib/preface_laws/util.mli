(** Some utils. *)

(** {1 Utils functions} *)

val swap : 'a * 'b -> 'b * 'a
val assoc : ('a * 'b) * 'c -> 'a * ('b * 'c)
val unassoc : 'a * ('b * 'c) -> ('a * 'b) * 'c
val swap_either : ('a, 'b) Either.t -> ('b, 'a) Either.t
val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c

val assoc_either :
  (('a, 'b) Either.t, 'c) Either.t -> ('a, ('b, 'c) Either.t) Either.t

val unassoc_either :
  ('a, ('b, 'c) Either.t) Either.t -> (('a, 'b) Either.t, 'c) Either.t

(** {1 Recurrent modules} *)

(** [Id] is [type 'a t = 'a]. *)

module Id : sig
  type 'a t = 'a

  module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t
  module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t
  module Monad : Preface_specs.MONAD with type 'a t = 'a t
end

(** A full profunctorial function module. *)

module Fun : sig
  type ('a, 'b) t = 'a -> 'b

  module Profunctor : Preface_specs.PROFUNCTOR with type ('a, 'b) t = ('a, 'b) t
  module Strong : Preface_specs.STRONG with type ('a, 'b) t = ('a, 'b) t
  module Choice : Preface_specs.CHOICE with type ('a, 'b) t = ('a, 'b) t
  module Closed : Preface_specs.CLOSED with type ('a, 'b) t = ('a, 'b) t
end

(** {1 Parametrized modules} *)

(** [Endo] is a function from ['a] to ['a]. *)

module Endo (T : Preface_specs.Types.T0) :
  Preface_specs.MONOID with type t = T.t -> T.t

(** The [Dual] of a monoid combines element in the opposite order. *)

module Dual (T : Preface_specs.MONOID) : Preface_specs.MONOID with type t = T.t
