(** A [Traversable] is a data structure that can be traversed from left to
    right, performing an action on each element.

    A common usage of [Traversable] is to turn any
    [Traversable.t of Applicative.t] into a [Applicative.t of Traversable.t].

    For example, going to ['a option list] to ['a list option]. *)

(** {1 Structure anatomy} *)

(** Standard requirement. *)
module type CORE = sig
  type 'a t
  (** The type holded by the [Traversable]. *)

  type 'a iter
  (** The iterable type holded by the [Traversable]. *)

  val traverse : ('a -> 'b t) -> 'a iter -> 'b iter t
  (** Map each element of a structure to an action, evaluate these actions from
      left to right, and collect the results. **)
end

(** Operations *)
module type OPERATION = sig
  type 'a t
  (** The type holded by the [Traversable]. *)

  type 'a iter
  (** The iterable type holded by the [Traversable]. *)

  val sequence : 'a t iter -> 'a iter t
  (** Evaluate each action in the structure from left to right, and collect the
      results *)
end

(** {1 API} *)

(** The complete interface of a [Traversable] *)
module type API = sig
  include CORE

  include OPERATION with type 'a t := 'a t and type 'a iter := 'a iter
end

(** {1 Bibliography}

    - {{:http://www.soi.city.ac.uk/~ross/papers/Applicative.html} Applicative
      Programming with Effects}
    - {{:http://web.comlab.ox.ac.uk/oucl/work/jeremy.gibbons/publications/#iterator}
      The Essence of the Iterator Pattern}
    - {{:http://arxiv.org/pdf/1202.2919} An Investigation of the Laws of
      Traversals} *)
