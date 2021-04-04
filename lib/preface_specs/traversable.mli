(** A [Traversable] is a data structure that can be traversed from left to
    right, performing an action on each element. *)

(** A common usage of [Traversable] is to turn any [Traversable] of
    {!module:Applicative} into a {!module:Applicative} of [Traversable].

    For example, going to ['a option list] to ['a list option]. *)

(** {1 Structure anatomy} *)

(** The minimum definition of a traversable. It is by using the combinators of
    this module that the other combinators will be derived. *)
module type CORE = sig
  type 'a t
  (** The type held by the [Traversable]. *)

  type 'a iter
  (** The iterable type held by the [Traversable]. *)

  val traverse : ('a -> 'b t) -> 'a iter -> 'b iter t
  (** Map each element of a structure to an action, evaluate these actions from
      left to right, and collect the results. **)
end

(** Additional operations. *)
module type OPERATION = sig
  type 'a t
  (** The type held by the [Traversable]. *)

  type 'a iter
  (** The iterable type held by the [Traversable]. *)

  val sequence : 'a t iter -> 'a iter t
  (** Evaluate each action in the structure from left to right, and collect the
      results *)
end

(** {1 Complete API} *)

(** The complete interface of a [Traversable] *)
module type API = sig
  include CORE
  (** @closed *)

  (** @closed *)
  include OPERATION with type 'a t := 'a t and type 'a iter := 'a iter
end

(** The complete interface of a [Traversable] over a {!module:Monad}*)
module type API_OVER_MONAD = sig
  include Monad.API
  (** @inline *)

  (** {1 Traversable using Monad form} *)

  module Traversable (M : Monad.API) :
    API with type 'a iter = 'a t and type 'a t = 'a M.t
end

(** The complete interface of a [Traversable] over an {!module:Applicative}*)
module type API_OVER_APPLICATIVE = sig
  include Applicative.API
  (** @inline *)

  (** {1 Traversable using Applicative form} *)

  module Traversable (A : Applicative.API) :
    API with type 'a iter = 'a t and type 'a t = 'a A.t
end

(** {1 Additional references}

    - {{:http://www.soi.city.ac.uk/~ross/papers/Applicative.html} Applicative
      Programming with Effects}
    - {{:http://web.comlab.ox.ac.uk/oucl/work/jeremy.gibbons/publications/#iterator}
      The Essence of the Iterator Pattern}
    - {{:http://arxiv.org/pdf/1202.2919} An Investigation of the Laws of
      Traversals} *)
