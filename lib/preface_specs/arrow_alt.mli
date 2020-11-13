(** An [Arrow] with a combine. *)

(** {1 Structure anatomy} *)

(** Combine operation. *)
module type COMBINE = sig
  type ('a, 'b) t
  (** The type held by the [Arrow_alt]. *)

  val combine : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  (** Combine two values of [('a, 'b) t] into one. *)
end

(** Requirement via [arrow] and [combine]. *)
module type CORE_WITH_ARROW = sig
  include COMBINE

  include Arrow.CORE_WITH_ARROW with type ('a, 'b) t := ('a, 'b) t
end

(** Requirement via [fst] and [combine]. *)
module type CORE_WITH_ARROW_AND_FST = sig
  include COMBINE

  include Arrow.CORE_WITH_ARROW_AND_FST with type ('a, 'b) t := ('a, 'b) t
end

(** Requirement via [split] and [combine]. *)
module type CORE_WITH_ARROW_AND_SPLIT = sig
  include COMBINE

  include Arrow.CORE_WITH_ARROW_AND_SPLIT with type ('a, 'b) t := ('a, 'b) t
end

(** Standard requirement *)
module type CORE = sig
  include COMBINE

  include Arrow.CORE with type ('a, 'b) t := ('a, 'b) t
end

(** Operations. *)
module type OPERATION = sig
  include Arrow.OPERATION

  val times : int -> ('a, 'b) t -> ('a, 'b) t option
  (** [times n x] apply [combine] on [x] [n] times. If [n] is lower than [1] the
      function will returns [None]. *)

  val reduce_nel : ('a, 'b) t Preface_core.Nonempty_list.t -> ('a, 'b) t
  (** Reduce a [Nonempty_list.t] using [combine]. *)
end

module type ALIAS = Arrow.ALIAS
(** Aliases of operations functions. *)

(** Infix operators. *)
module type INFIX = sig
  include Arrow.INFIX

  val ( <|> ) : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  (** Infix version of {!val:CORE.combine} *)
end

(** {1 API} *)

(** The complete interface of an [Arrow_alt]. *)
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
