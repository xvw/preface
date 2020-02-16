(** An {!Applicative} parametrized by 2 types *)

(** {1 Structure anatomy} *)

(** Requirement via [map] and [product]. *)
module type CORE_WITH_MAP_AND_PRODUCT = sig
  type ('a, 'b) t
  (** The type holded by the [Applicative2]. *)

  val pure : 'b -> ('a, 'b) t
  (** Create a new [('a, 'b, 'c) t]. *)

  val map : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  (** Mapping over from ['b] to ['c] over [('a, 'b) t] to [('a, 'c) t]. *)

  val product : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
  (** Product functor mapping from [('a, 'b) t] and [('a, 'c) t] to
      [('a, ('b * 'c)) t]. *)
end

(** Requirement via [apply]. *)
module type CORE_WITH_APPLY = sig
  type ('a, 'b) t
  (** The type holded by the [Applicative2]. *)

  val pure : 'b -> ('a, 'b) t
  (** Create a new [('a, 'b) t]. *)

  val apply : ('a, 'b -> 'c) t -> ('a, 'b) t -> ('a, 'c) t
  (** Applicative2 functor of [('a, 'b -> 'c) t] over [('a, 'b) t] to
      [('a, 'c) t]. *)
end

(** Standard requirement. *)
module type CORE = sig
  include CORE_WITH_APPLY

  include CORE_WITH_MAP_AND_PRODUCT with type ('a, 'b) t := ('a, 'b) t
end

(** Operations *)
module type OPERATION = sig
  type ('a, 'b) t
  (** The type holded by the [Applicative2]. *)

  val lift : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  (** Mapping over from ['b] to ['c] over [('a, 'b) t] to [('a, 'c) t]. *)

  val lift2 : ('b -> 'c -> 'd) -> ('a, 'b) t -> ('a, 'c) t -> ('a, 'd) t
  (** Mapping over from ['b] and ['c] to ['d] over [('a, 'b) t] and [('a, 'c) t]
      to [('a, 'd) t]. *)

  val lift3 :
       ('b -> 'c -> 'd -> 'e)
    -> ('a, 'b) t
    -> ('a, 'c) t
    -> ('a, 'd) t
    -> ('a, 'e) t
  (** Mapping over from ['b] and ['c] and ['d] to ['e] over [('a, 'b) t] and
      [('a, 'c) t] and [('a, 'd) t] to [('a, 'e) t]. *)
end

(** Syntax extensions *)
module type SYNTAX = sig
  type ('a, 'b) t
  (** The type holded by the [Applicative2]. *)

  val ( let+ ) : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
  (** Flipped mapping over from ['b] to ['c] over [('a, 'b) t] to [('a, 'c) t]. *)

  val ( and+ ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
  (** Product functor mapping from [('a, 'b) t] and [('a, 'c) t] to
      [('a, ('b * 'c)) t]. *)
end

(** Infix notations *)
module type INFIX = sig
  type ('a, 'b) t
  (** The type holded by the [Applicative2]. *)

  val ( <$> ) : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  (** Infix version of {!val:CORE.map}. *)

  val ( <*> ) : ('a, 'b -> 'c) t -> ('a, 'b) t -> ('a, 'c) t
  (** Infix version of {!val:CORE.apply} *)

  val ( <**> ) : ('a, 'b) t -> ('a, 'b -> 'c) t -> ('a, 'c) t
  (** Infix flipped version of {!val:CORE.apply} *)

  val ( *> ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'c) t
  (** Discard the value of the first argument. *)

  val ( <* ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b) t
  (** Discard the value of the second argument. *)
end

(** {1 API} *)

(** The complete interface of an [Applicative2]. *)
module type API = sig
  include CORE

  include OPERATION with type ('a, 'b) t := ('a, 'b) t

  module Syntax : SYNTAX with type ('a, 'b) t := ('a, 'b) t

  include module type of Syntax

  module Infix : INFIX with type ('a, 'b) t := ('a, 'b) t

  include module type of Infix
end

(** {1 Bibliography}

    - {{:http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Applicative.html}
      Haskell's documentation of an Applicative Functor}
    - {{:http://www.staff.city.ac.uk/~ross/papers/Applicative.html} Applicative
      Programming with Effects} *)
