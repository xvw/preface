(** Some utils. *)

(** {1 Recurrent modules} *)

(** [Id] is [type 'a t = 'a]. *)

module Id : sig
  type 'a t = 'a

  module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t
  module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t
  module Monad : Preface_specs.MONAD with type 'a t = 'a t
end

(** {1 Parametrized modules} *)

(** [Endo] is a function from ['a] to ['a]. *)

module Endo (T : Preface_specs.Types.T0) :
  Preface_specs.MONOID with type t = T.t -> T.t

(** The [Dual] of a monoid combines element in the opposite order. *)

module Dual (T : Preface_specs.MONOID) : Preface_specs.MONOID with type t = T.t
