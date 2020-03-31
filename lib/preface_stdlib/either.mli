(** Exposes [Either.t] *)

(** {1 Types} *)

type ('a, 'b) t =
  | Left of 'a
  | Right of 'b

(** {1 Implementation} *)

module Bifunctor : Preface_specs.BIFUNCTOR with type ('a, 'b) t = ('a, 'b) t
(** {2 Bifunctor API} *)

(** {2 Functor API} *)
module Functor (T : sig
  type t
end) : Preface_specs.FUNCTOR with type 'a t = (T.t, 'a) Bifunctor.t

(** {2 Applicative API} *)
module Applicative (T : sig
  type t
end) : Preface_specs.APPLICATIVE with type 'a t = (T.t, 'a) Bifunctor.t

(** {2 Monad API} *)
module Monad (T : sig
  type t
end) : Preface_specs.MONAD with type 'a t = (T.t, 'a) Bifunctor.t

(** {1 Helpers} *)

val pure : 'b -> ('a, 'b) t
(** Create a value from ['b] to [('a, 'b) t]. *)

include Preface_specs.Requirements.EITHER with type ('a, 'b) t := ('a, 'b) t
