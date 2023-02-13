(** A [Comonad] is the dual of the {!module:Monad}. *)

(** {2 Laws}

    - [extend extract = id]
    - [(extend %> extract) f = f]
    - [extend g %> extend f = extend (extend g %> f)]
    - [f =>= extract = f]
    - [extract =>= f = f]
    - [(f =>= g) =>= h = f =>= (g =>= h)]
    - [extract <% duplicate = id]
    - [fmap extract <% duplicate = id]
    - [duplicate %> duplicate = fmap duplicate <% duplicate]
    - [extend f = fmap f <% duplicate]
    - [duplicate = extend id]
    - [fmap f = extend (f <% extract)] *)

(** {1 Minimal definition} *)

(** Minimal definition using [extract], [map] and [duplicate]. *)
module type WITH_MAP_AND_DUPLICATE = sig
  type 'a t
  (** The type held by the [Comonad]. *)

  include Indexed_comonad.WITH_MAP_AND_DUPLICATE with type ('a, _) t := 'a t
  (** @inline *)
end

(** Minimal definition using [extract] and [extend]. *)
module type WITH_EXTEND = sig
  type 'a t
  (** The type held by the [Comonad]. *)

  include Indexed_comonad.WITH_EXTEND with type ('a, _) t := 'a t
  (** @inline *)
end

(** Minimal definition using [extract] and [compose_left_to_right]. *)
module type WITH_COKLEISLI_COMPOSITION = sig
  type 'a t
  (** The type held by the [Comonad]. *)

  include Indexed_comonad.WITH_COKLEISLI_COMPOSITION with type ('a, _) t := 'a t
  (** @inline *)
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  type 'a t
  (** The type held by the [Comonad]. *)

  include Indexed_comonad.CORE with type ('a, _) t := 'a t
  (** @inline *)
end

(** Additional operations. *)
module type OPERATION = sig
  type 'a t
  (** The type held by the [Comonad]. *)

  include Indexed_comonad.OPERATION with type ('a, _) t := 'a t
  (** @inline *)
end

(** Syntax extensions. *)
module type SYNTAX = sig
  type 'a t
  (** The type held by the [Comonad]. *)

  include Indexed_comonad.SYNTAX with type ('a, _) t := 'a t
  (** @inline *)
end

(** Infix operators. *)
module type INFIX = sig
  type 'a t
  (** The type held by the [Comonad]. *)

  include Indexed_comonad.INFIX with type ('a, _) t := 'a t
  (** @inline *)
end

(** {1 Complete API} *)

(** The complete interface of a [Comonad]. *)
module type API = sig
  (** {1 Type} *)

  type 'a t
  (** The type held by the [Comonad]. *)

  include Indexed_comonad.API with type ('a, _) t := 'a t
  (** @inline *)
end

(** {1 Additional interfaces} *)

(** {2 Transformer}

    A standard representation of a comonad transformer. (It is likely that not
    all transformers respect this interface) *)

module type TRANSFORMER = sig
  type 'a comonad
  (** The inner comonad. *)

  type 'a t
  (** The type held by the comonad transformer.*)

  val lower : 'a t -> 'a comonad
  (** get the underlying comonad. *)
end
