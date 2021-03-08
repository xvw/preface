(** {1 Capabilities}

    - {!val:Functor}
    - {!val:Applicative}
    - {!val:Monad}

    {1 Use cases} *)

module Over
    (S : Preface_specs.Types.T0)
    (P : Preface_specs.MONOID) (R : sig
      val right : P.t -> S.t -> S.t
    end) : sig
  type env = S.t

  type output = P.t

  type 'a t

  module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t
  (** {2 Functor API} *)

  module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t
  (** {2 Applicative API} *)

  module Monad : Preface_specs.MONAD with type 'a t = 'a t
  (** {2 Monad API} *)
end

(** {1 Bibliography}

    - {{:https://danel.ahman.ee/papers/types13postproc.pdf}
      Update Monads: Cointerpreting Directed Containers} *)
