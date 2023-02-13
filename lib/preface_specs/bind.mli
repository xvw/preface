(** A [Bind] allow to sequences operations that are dependent from one to
    another, in contrast to {!module:Applicative}, which executes a series of
    independent actions. It is a Monad without [return] operation. *)

(** {2 Laws}

    To have a predictable behaviour, the instance of [Bind] must obey some laws.

    + [(m >>= f) >>= g = m >>= (fun x +> f x >>= g)]
    + [join % join = join % (map join)]
    + [map id = id]
    + [map (g % f) = map g % map f]
    + [map f % join = join % map (map f)]
    + [map f % pure = pure % f]
    + [(f >=> g) >=> h = f >=> (g >=> h)] *)

(** {1 Minimal definition} *)

(** Minimal definition using [bind]. *)
module type WITH_BIND = sig
  type 'a t
  (** The type held by the [Bind]. *)

  include Indexed_bind.WITH_BIND with type ('a, _) t := 'a t
  (** @inline *)
end

(** Minimal definition using [map] and [join]. *)
module type WITH_MAP_AND_JOIN = sig
  type 'a t
  (** The type held by the [Bind]. *)

  include Indexed_bind.WITH_MAP_AND_JOIN with type ('a, _) t := 'a t
  (** @inline *)
end

(** Minimal definition using [compose_left_to_right]. *)
module type WITH_KLEISLI_COMPOSITION = sig
  type 'a t
  (** The type held by the [Bind]. *)

  include Indexed_bind.WITH_KLEISLI_COMPOSITION with type ('a, _) t := 'a t
  (** @inline *)
end

(** Minimal definition using [map] and [bind]. *)
module type WITH_MAP_AND_BIND = sig
  type 'a t
  (** The type held by the [Bind]. *)

  include Indexed_bind.WITH_MAP_AND_BIND with type ('a, _) t := 'a t
  (** @inline *)
end

(** Minimal definition using [map] and [compose_left_to_right]. *)
module type WITH_MAP_AND_KLEISLI_COMPOSITION = sig
  type 'a t
  (** The type held by the [Bind]. *)

  include
    Indexed_bind.WITH_MAP_AND_KLEISLI_COMPOSITION with type ('a, _) t := 'a t
  (** @inline *)
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  type 'a t
  (** The type held by the [Bind]. *)

  include Indexed_bind.CORE with type ('a, _) t := 'a t
  (** @inline *)
end

(** Additional operations. *)
module type OPERATION = sig
  type 'a t
  (** The type held by the [Bind]. *)

  include Indexed_bind.OPERATION with type ('a, _) t := 'a t
  (** @inline *)
end

(** Syntax extensions. *)
module type SYNTAX = sig
  type 'a t
  (** The type held by the [Bind]. *)

  include Indexed_bind.SYNTAX with type ('a, _) t := 'a t
  (** @inline *)
end

(** Infix operators. *)
module type INFIX = sig
  type 'a t
  (** The type held by the [Bind]. *)

  include Indexed_bind.INFIX with type ('a, _) t := 'a t
  (** @inline *)
end

(** {1 Complete API} *)

(** The complete interface of a [Bind]. *)
module type API = sig
  (** {1 Type} *)

  type 'a t
  (** The type held by the [Bind]. *)

  include Indexed_bind.API with type ('a, _) t := 'a t
  (** @inline *)
end

(** {1 Additional references}

    - {{:https://hackage.haskell.org/package/semigroupoids-5.3.6/docs/Data-Functor-Bind.html}
      Haskell's documentation of Bind} *)
