(** {1 Capabilities}

    - {!val:Functor}
    - {!val:Applicative}
    - {!val:Monad}

    {1 Use cases} *)

module Over (R : sig
  module S : Preface_specs.Types.T0

  module P : Preface_specs.MONOID

  val right : P.t -> S.t -> S.t
end) : sig
  type 'a t

  module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t
  (** {2 Applicative API} *)

  module Monad : Preface_specs.MONAD with type 'a t = 'a t
  (** {2 Monad API} *)
end
