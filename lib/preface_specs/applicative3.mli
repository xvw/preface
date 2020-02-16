(** An {!Applicative} parametrized by 3 types *)

(** {1 Structure anatomy} *)

(** Requirement via [map] and [product]. *)
module type CORE_WITH_MAP_AND_PRODUCT = sig
  type ('a, 'b, 'c) t
  (** The type holded by the [Applicative3]. *)

  val pure : 'c -> ('a, 'b, 'c) t
  (** Create a new [('a, 'b, 'c) t]. *)

  val map : ('c -> 'd) -> ('a, 'b, 'c) t -> ('a, 'b, 'd) t
  (** Mapping over from ['c] to ['d] over [('a, 'b, 'c) t] to [('a, 'b, 'd) t]. *)

  val product : ('a, 'b, 'c) t -> ('a, 'b, 'd) t -> ('a, 'b, 'c * 'd) t
  (** Product functor mapping from [('a, 'b, 'c) t] and [('a, 'b, 'd) t] to
      [('a, 'b, ('c * 'd)) t]. *)
end

(** Requirement via [apply]. *)
module type CORE_WITH_APPLY = sig
  type ('a, 'b, 'c) t
  (** The type holded by the [Applicative3]. *)

  val pure : 'c -> ('a, 'b, 'c) t
  (** Create a new [('a, 'b, 'c) t]. *)

  val apply : ('a, 'b, 'c -> 'd) t -> ('a, 'b, 'c) t -> ('a, 'b, 'd) t
  (** Applicative3 functor of [('a, 'b, 'c -> 'd) t] over [('a, 'b, 'c) t] to
      [('a, 'b, 'd) t]. *)
end

(** Standard requirement. *)
module type CORE = sig
  include CORE_WITH_APPLY

  include CORE_WITH_MAP_AND_PRODUCT with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
end

(** Operations *)
module type OPERATION = sig
  type ('a, 'b, 'c) t
  (** The type holded by the [Applicative3]. *)

  val lift : ('c -> 'd) -> ('a, 'b, 'c) t -> ('a, 'b, 'd) t
  (** Mapping over from ['c] to ['d] over [('a, 'b, 'c) t] to [('a, 'b, 'd) t]. *)

  val lift2 :
    ('c -> 'd -> 'e) -> ('a, 'b, 'c) t -> ('a, 'b, 'd) t -> ('a, 'b, 'e) t
  (** Mapping over from ['c] and ['d] to ['e] over [('a, 'b, 'c) t] and
      [('a, 'b, 'd) t] to [('a, 'b, 'e) t]. *)

  val lift3 :
       ('c -> 'd -> 'e -> 'f)
    -> ('a, 'b, 'c) t
    -> ('a, 'b, 'd) t
    -> ('a, 'b, 'e) t
    -> ('a, 'b, 'f) t
  (** Mapping over from ['c] and ['d] and ['e] to ['f] over [('a, 'b, 'c) t] and
      [('a, 'b, 'd) t] and [('a, 'b, 'e) t] to [('a, 'b, 'f) t]. *)
end

(** Syntax extensions *)
module type SYNTAX = sig
  type ('a, 'b, 'c) t
  (** The type holded by the [Applicative3]. *)

  val ( let+ ) : ('a, 'b, 'c) t -> ('c -> 'd) -> ('a, 'b, 'd) t
  (** Flipped mapping over from ['c] to ['d] over [('a, 'b, 'c) t] to
      [('a, 'b, 'd) t]. *)

  val ( and+ ) : ('a, 'b, 'c) t -> ('a, 'b, 'd) t -> ('a, 'b, 'c * 'd) t
  (** Product functor mapping from [('a, 'b, 'c) t] and [('a, 'b, 'd) t] to
      [('a, 'b, ('c * 'd)) t]. *)
end

(** Infix notations *)
module type INFIX = sig
  type ('a, 'b, 'c) t
  (** The type holded by the [Applicative3]. *)

  val ( <$> ) : ('c -> 'd) -> ('a, 'b, 'c) t -> ('a, 'b, 'd) t
  (** Infix version of {!val:CORE.map}. *)

  val ( <*> ) : ('a, 'b, 'c -> 'd) t -> ('a, 'b, 'c) t -> ('a, 'b, 'd) t
  (** Infix version of {!val:CORE.apply} *)

  val ( <**> ) : ('a, 'b, 'c) t -> ('a, 'b, 'c -> 'd) t -> ('a, 'b, 'd) t
  (** Infix flipped version of {!val:CORE.apply} *)

  val ( *> ) : ('a, 'b, 'c) t -> ('a, 'b, 'd) t -> ('a, 'b, 'd) t
  (** Discard the value of the first argument. *)

  val ( <* ) : ('a, 'b, 'c) t -> ('a, 'b, 'd) t -> ('a, 'b, 'c) t
  (** Discard the value of the second argument. *)
end

(** {1 API} *)

(** The complete interface of an [Applicative3]. *)
module type API = sig
  include CORE

  include OPERATION with type ('a, 'b, 'c) t := ('a, 'b, 'c) t

  module Syntax : SYNTAX with type ('a, 'b, 'c) t := ('a, 'b, 'c) t

  include module type of Syntax

  module Infix : INFIX with type ('a, 'b, 'c) t := ('a, 'b, 'c) t

  include module type of Infix
end

(** {1 Bibliography}

    - {{:http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Applicative.html}
      Haskell's documentation of an Applicative Functor}
    - {{:http://www.staff.city.ac.uk/~ross/papers/Applicative.html} Applicative
      Programming with Effects} *)
