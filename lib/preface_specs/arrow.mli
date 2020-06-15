(** An [Arrow] is an abstract view of computation sitting between [Applicative]
    and [Monad]. *)

(** {1 Structure anatomy} *)

(** Requirement via [arrow]. *)
module type CORE_WITH_ARROW = sig
  type ('a, 'b) t
  (** The type held by the [Arrow]. *)

  val arrow : ('a -> 'b) -> ('a, 'b) t
  (** Lift a function to an Arrow. *)
end

(** Requirement via [fst]. *)
module type CORE_WITH_ARROW_AND_FST = sig
  include CORE_WITH_ARROW

  val fst : ('a, 'b) t -> ('a * 'd, 'b * 'd) t
  (** Send the first component of the input through the argument arrow, and copy
      the rest unchanged to the output. *)
end

(** Requirement via [split]. *)
module type CORE_WITH_ARROW_AND_SPLIT = sig
  include CORE_WITH_ARROW

  val split : ('a, 'b) t -> ('c, 'd) t -> ('a * 'c, 'b * 'd) t
  (** Split the input between the two argument arrows and combine their output. *)
end

(** Standard requirement *)
module type CORE = sig
  type ('a, 'b) t
  (** The type held by the [Arrow]. *)

  include Category.CORE with type ('a, 'b) t := ('a, 'b) t

  include CORE_WITH_ARROW_AND_FST with type ('a, 'b) t := ('a, 'b) t

  include CORE_WITH_ARROW_AND_SPLIT with type ('a, 'b) t := ('a, 'b) t
end

(** Operations. *)
module type OPERATION = sig
  type ('a, 'b) t
  (** The type held by the [Arrow]. *)

  include Category.OPERATION with type ('a, 'b) t := ('a, 'b) t

  val return : unit -> ('a, 'a) t
  (** Represent the identity Arrow. *)

  val snd : ('a, 'b) t -> ('d * 'a, 'd * 'b) t
  (** Send the second component of the input through the argument arrow, and
      copy the rest unchanged to the output. *)

  val fan_out : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
  (** Send the input to both argument arrows and combine their output. *)

  val pre_compose_left_to_right : ('a -> 'b) -> ('b, 'c) t -> ('a, 'c) t
  (** Precomposition with a function (the function should be pure). *)

  val post_compose_left_to_right : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
  (** Postcomposition with a function (the function should be pure). *)

  val pre_compose_right_to_left : ('b, 'c) t -> ('a -> 'b) -> ('a, 'c) t
  (** Reversed version of {!val:pre_compose_left_to_right}. *)

  val post_compose_right_to_left : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  (** Reversed version of {!val:post_compose_left_to_right}. *)
end

(** Aliases of operations functions. *)
module type ALIAS = sig
  type ('a, 'b) t
  (** The type held by the [Arrow]. *)

  val pre_compose : ('a -> 'b) -> ('b, 'c) t -> ('a, 'c) t
  (** Conveinent alias of {!val:OPERATION.pre_compose_left_to_right}. *)

  val post_compose : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
  (** Conveinent alias of {!val:OPERATION.post_compose_left_to_right}. *)
end

(** Infix operators. *)
module type INFIX = sig
  type ('a, 'b) t
  (** The type held by the [Arrow]. *)

  include Category.INFIX with type ('a, 'b) t := ('a, 'b) t

  val ( *** ) : ('a, 'b) t -> ('c, 'd) t -> ('a * 'c, 'b * 'd) t
  (** Infix version of {!val:CORE.split}. *)

  val ( &&& ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
  (** Infix version of {!val:OPERATION.fan_out}. *)

  val ( ^>> ) : ('a -> 'b) -> ('b, 'c) t -> ('a, 'c) t
  (** Infix version of {!val:OPERATION.pre_compose_left_to_right}. *)

  val ( >>^ ) : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
  (** Infix version of {!val:OPERATION.post_compose_left_to_right}. *)

  val ( <<^ ) : ('b, 'c) t -> ('a -> 'b) -> ('a, 'c) t
  (** Infix version of {!val:OPERATION.post_compose_right_to_left}. *)

  val ( ^<< ) : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  (** Infix version of {!val:OPERATION.post_compose_right_to_left}. *)
end

(** {1 API} *)

(** The complete interface of an [Arrow]. *)
module type API = sig
  include CORE

  include OPERATION with type ('a, 'b) t := ('a, 'b) t

  include ALIAS with type ('a, 'b) t := ('a, 'b) t

  module Infix : INFIX with type ('a, 'b) t = ('a, 'b) t

  include INFIX with type ('a, 'b) t := ('a, 'b) t
end

(** {1 Bibliography}

    - {{:https://www.haskell.org/arrows/} Arrows: A General Interface to
      Computation}
    - {{:https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Arrow.html}
      Haskell's documentation of Arrow} *)
