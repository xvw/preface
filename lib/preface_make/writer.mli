(** {1 Capabilities}

    - {!val:Functor}
    - {!val:Applicative}
    - {!val:Monad}

    {1 Use cases}

    The Writer module gives you the ability to accumulate values thanks to the
    parametric `Monoid`. Then for instance it can be used to create a log reflecting
    performed operations.

    {1 Example} *)

module Over (M : Preface_specs.MONOID) : sig
  type 'a t

  val run : 'a t -> ('a * M.t)

  module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t
  (** {2 Functor API} *)

  module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t
  (** {2 Applicative API} *)

  module Monad : Preface_specs.MONAD with type 'a t = 'a t
  (** {2 Monad API} *)
end
