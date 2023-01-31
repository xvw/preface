(** A [Monad] allow to sequences operations that are dependent from one to
    another, in contrast to {!module:Applicative}, which executes a series of
    independent actions.*)

(** {2 Laws}

    To have a predictable behaviour, the instance of [Monad] must obey some
    laws.

    + [return a >>= f = f a]
    + [m >>= return = m]
    + [(m >>= f) >>= g = m >>= (fun x +> f x >>= g)]
    + [join % join = join % (map join)]
    + [join % return = id = join % map pure]
    + [map id = id]
    + [map (g % f) = map g % map f]
    + [map f % join = join % map (map f)]
    + [map f % pure = pure % f]
    + [return >=> g = g]
    + [f >=> return = f]
    + [(f >=> g) >=> h = f >=> (g >=> h)] *)

(** {1 Minimal definition} *)

(** Minimal interface using [map] and [product]. *)
module type WITH_RETURN = sig
  type 'a t
  (** The type held by the [Monad]. *)

  include Indexed_monad.WITH_RETURN with type ('a, _) t := 'a t
  (** @inline *)
end

(** Minimal definition using [return] and [bind]. *)
module type WITH_RETURN_AND_BIND = sig
  type 'a t
  (** The type held by the [Monad]. *)

  include Indexed_monad.WITH_RETURN_AND_BIND with type ('a, _) t := 'a t
  (** @inline *)
end

(** Minimal definition using [return], [map] and [join]. *)
module type WITH_RETURN_MAP_AND_JOIN = sig
  type 'a t
  (** The type held by the [Monad]. *)

  include Indexed_monad.WITH_RETURN_MAP_AND_JOIN with type ('a, _) t := 'a t
  (** @inline *)
end

(** Minimal definition using [return] and [compose_left_to_right]. *)
module type WITH_RETURN_AND_KLEISLI_COMPOSITION = sig
  type 'a t
  (** The type held by the [Monad]. *)

  include
    Indexed_monad.WITH_RETURN_AND_KLEISLI_COMPOSITION
      with type ('a, _) t := 'a t
  (** @inline *)
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  type 'a t
  (** The type held by the [Monad]. *)

  include Indexed_monad.CORE with type ('a, _) t := 'a t
  (** @inline *)
end

(** Additional operations. *)
module type OPERATION = sig
  type 'a t
  (** The type held by the [Monad]. *)

  include Indexed_monad.OPERATION with type ('a, _) t := 'a t
  (** @inline *)
end

(** Syntax extensions. *)
module type SYNTAX = sig
  type 'a t
  (** The type held by the [Monad]. *)

  include Indexed_monad.SYNTAX with type ('a, _) t := 'a t
  (** @inline *)
end

(** Infix operators. *)
module type INFIX = sig
  type 'a t
  (** The type held by the [Monad]. *)

  include Indexed_monad.INFIX with type ('a, _) t := 'a t
  (** @inline *)
end

(** {1 Complete API} *)

(** The complete interface of a [Monad]. *)
module type API = sig
  (** {1 Type} *)

  type 'a t
  (** The type held by the [Monad]. *)

  (** {1 Functions} *)

  include CORE with type 'a t := 'a t
  (** @inline *)

  include OPERATION with type 'a t := 'a t
  (** @inline *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type 'a t = 'a t

  include INFIX with type 'a t := 'a t
  (** @inline *)

  (** {1 Syntax} *)

  module Syntax : SYNTAX with type 'a t = 'a t

  include SYNTAX with type 'a t := 'a t
  (** @inline *)
end

(** {1 Additional interfaces} *)

(** {2 Transformer}

    A standard representation of a monad transformer. (It is likely that not all
    transformers respect this interface) *)

module type TRANSFORMER = sig
  type 'a monad
  (** The inner monad. *)

  type 'a t
  (** The type held by the monad transformer.*)

  val upper : 'a monad -> 'a t
  (** promote monad into the transformation. *)
end

(** {1 Additional references}

    - {{:http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad.html}
      Haskell's documentation of a Monad}
    - {{:http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf}
      Monad for functional programming}
    - {{:https://person.dibris.unige.it/moggi-eugenio/ftp/ic91.pdf} Notions of
      computations and monads}
    - {{:https://wiki.haskell.org/All_About_Monads} All About Monads} *)
