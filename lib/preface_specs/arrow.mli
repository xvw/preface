(** An [Arrow] is an abstract view of computation sitting between
    {!module:Applicative} and {!module:Monad}. [Arrow] is built on the top of
    {!module:Category} and {!module:Strong}. So an [Arrow] is also a
    {!module:Category}. *)

(** {2 Laws}

    To have a predictable behaviour, the instance of [Arrow] must obey some
    laws.

    + All {!module:Category} laws
    + [arrow id = id]
    + [arrow (g % f) = arrow f >>> arrow g]
    + [fst (arrow f) = arr (fst f)]
    + [fst (f >>> g) = fst f >>> fst g]
    + [fst f >>> arrow (fun (x,y) -> (x,g y)) = arrow (fun (x,y) -> (x,g y)) >>> fst f]
    + [fst f >>> arrow Stdlib.fst = arrow Stdlib.fst >>> f]
    + [fst (fst f) >>> arrow assoc = arrow assoc >>> fst f] *)

(** {1 Minimal definition} *)

(** Exposes the [arrow] function, mandatory for each requirement. *)
module type WITH_ARROW = sig
  type ('a, 'b) t
  (** The type held by the [Arrow]. *)

  val arrow : ('a -> 'b) -> ('a, 'b) t
  (** Lift a function to an [Arrow]. *)
end

(** Minimal definition using [fst]. *)
module type WITH_ARROW_AND_FST = sig
  include WITH_ARROW
  (** @inline *)

  val fst : ('a, 'b) t -> ('a * 'd, 'b * 'd) t
  (** Send the first component of the input through the argument [Arrow], and
      copy the rest unchanged to the output. *)
end

(** Minimal definition using [split]. *)
module type WITH_ARROW_AND_SPLIT = sig
  include WITH_ARROW
  (** @inline *)

  val split : ('a, 'b) t -> ('c, 'd) t -> ('a * 'c, 'b * 'd) t
  (** Split the input between the two given [Arrows] and combine their output. *)
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  type ('a, 'b) t
  (** The type held by the [Arrow]. *)

  include Category.CORE with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)

  include WITH_ARROW_AND_FST with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)

  include WITH_ARROW_AND_SPLIT with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)
end

(** Additional operations. *)
module type OPERATION = sig
  type ('a, 'b) t
  (** The type held by the [Arrow]. *)

  include Category.OPERATION with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)

  val return : unit -> ('a, 'a) t
  (** Represent the identity [Arrow]. *)

  val snd : ('a, 'b) t -> ('d * 'a, 'd * 'b) t
  (** Send the second component of the input through the given [Arrow], and copy
      the rest unchanged to the output. *)

  val fan_out : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
  (** Send the input to both argument arrows and combine their output. *)

  val pre_compose_left_to_right : ('a -> 'b) -> ('b, 'c) t -> ('a, 'c) t
  (** Precomposition with a function (the function should be pure). *)

  val post_compose_left_to_right : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
  (** Postcomposition with a function (the function should be pure). *)

  val pre_compose_right_to_left : ('b, 'c) t -> ('a -> 'b) -> ('a, 'c) t
  (** Reversed version of
      {!val:Preface_specs.Arrow.OPERATION.pre_compose_left_to_right}. *)

  val post_compose_right_to_left : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  (** Reversed version of
      {!val:Preface_specs.Arrow.OPERATION.post_compose_left_to_right}. *)
end

(** Aliases of some operations functions. *)
module type ALIAS = sig
  type ('a, 'b) t
  (** The type held by the [Arrow]. *)

  val pre_compose : ('a -> 'b) -> ('b, 'c) t -> ('a, 'c) t
  (** Conveinent alias of
      {!val:Preface_specs.Arrow.OPERATION.pre_compose_left_to_right}. *)

  val post_compose : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
  (** Conveinent alias of
      {!val:Preface_specs.Arrow.OPERATION.post_compose_left_to_right}. *)
end

(** Infix operators. *)
module type INFIX = sig
  type ('a, 'b) t
  (** The type held by the [Arrow]. *)

  include Category.INFIX with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)

  val ( *** ) : ('a, 'b) t -> ('c, 'd) t -> ('a * 'c, 'b * 'd) t
  (** Infix version of {!val:Preface_specs.Arrow.CORE.split}. *)

  val ( &&& ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
  (** Infix version of {!val:Preface_specs.Arrow.OPERATION.fan_out}. *)

  val ( ^>> ) : ('a -> 'b) -> ('b, 'c) t -> ('a, 'c) t
  (** Infix version of
      {!val:Preface_specs.Arrow.OPERATION.pre_compose_left_to_right}. *)

  val ( >>^ ) : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
  (** Infix version of
      {!val:Preface_specs.Arrow.OPERATION.post_compose_left_to_right}. *)

  val ( <<^ ) : ('b, 'c) t -> ('a -> 'b) -> ('a, 'c) t
  (** Infix version of
      {!val:Preface_specs.Arrow.OPERATION.post_compose_right_to_left}. *)

  val ( ^<< ) : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  (** Infix version of
      {!val:Preface_specs.Arrow.OPERATION.post_compose_right_to_left}. *)
end

(** {1 Complete API} *)

(** The complete interface of an [Arrow]. *)
module type API = sig
  (** {1 Type} *)

  type ('a, 'b) t
  (** The type held by the [Arrow]. *)

  (** {1 Functions} *)

  include CORE with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)

  include OPERATION with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type ('a, 'b) t = ('a, 'b) t

  include INFIX with type ('a, 'b) t := ('a, 'b) t
  (** @inline *)
end

(** {1 Additional references}

    - {{:http://www.cse.chalmers.se/~rjmh/Papers/arrows.pdf} Generalising Monads
      to Arrows}
    - {{:https://www.haskell.org/arrows/} Arrows: A General Interface to
      Computation}
    - {{:https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Arrow.html}
      Haskell's documentation of Arrow} *)
