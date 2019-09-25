(** A [Monad] - TODO *)

module type CORE_VIA_BIND = sig
  type 'a t
  (** The type holded by the [Monad]. *)

  val return : 'a -> 'a t
  (** Create a new ['a t]. *)

  val bind : ('a -> 'b t) -> 'a t -> 'b t
end

module type CORE_VIA_MAP_AND_JOIN = sig
  type 'a t
  (** The type holded by the [Monad]. *)

  val return : 'a -> 'a t
  (** Create a new ['a t]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** Mapping over from ['a] to ['b] over ['a t] to ['b t]. *)

  val join : 'a t t -> 'a t
end

module type CORE_VIA_KLEISLI_COMPOSITION = sig
  type 'a t
  (** The type holded by the [Monad]. *)

  val return : 'a -> 'a t
  (** Create a new ['a t]. *)

  val compose_left_to_right : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t
end

module type CORE = sig
  include CORE_VIA_BIND

  include CORE_VIA_MAP_AND_JOIN with type 'a t := 'a t

  include CORE_VIA_KLEISLI_COMPOSITION with type 'a t := 'a t
end

module type OPERATION = sig
  type 'a t
  (** The type holded by the [Monad]. *)

  val void : 'a t -> unit t

  val compose_right_to_left : ('b -> 'c t) -> ('a -> 'b t) -> 'a -> 'c t

  val lift : ('a -> 'b) -> 'a t -> 'b t
  (** Mapping over from ['a] to ['b] over ['a t] to ['b t]. *)

  val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** Mapping over from ['a] and ['b] to ['c] over ['a t] and
      ['b t] to ['c t].
  *)

  val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  (** Mapping over from ['a] and ['b] and ['c] to ['d] over ['a t]
      and ['b t] and ['c t] to ['d t].
  *)
end

module type SYNTAX = sig
  type 'a t
  (** The type holded by the [Monad]. *)

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

module type INFIX = sig
  type 'a t
  (** The type holded by the [Monad]. *)

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( =<< ) : ('a -> 'b t) -> 'a t -> 'b t

  val ( >=> ) : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t

  val ( <=< ) : ('b -> 'c t) -> ('a -> 'b t) -> 'a -> 'c t

  val ( >> ) : 'a t -> 'b t -> 'b t

  val ( << ) : 'a t -> 'b t -> 'a t
end

(** {1 API} *)

(** The complete interface of a [Monad]. *)
module type API = sig
  include CORE

  include OPERATION with type 'a t := 'a t

  module Syntax : SYNTAX with type 'a t := 'a t

  include module type of Syntax

  module Infix : INFIX with type 'a t := 'a t

  include module type of Infix
end
