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

(** Minimal definition using [bind]. *)
module type WITH_BIND = sig
  type 'a t
  (** The type held by the [Monad]. *)

  val return : 'a -> 'a t
  (** Create a new ['a t]. *)

  val bind : ('a -> 'b t) -> 'a t -> 'b t
  (** [bind f m] passes the result of computation [m] to function [f]. *)
end

(** Minimal definition using [map] and [join]. *)
module type WITH_MAP_AND_JOIN = sig
  type 'a t
  (** The type held by the [Monad]. *)

  val return : 'a -> 'a t
  (** Create a new ['a t]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** Mapping over from ['a] to ['b] over ['a t] to ['b t]. *)

  val join : 'a t t -> 'a t
  (** [join] remove one level of monadic structure, projecting its bound
      argument into the outer level. *)
end

(** Minimal definition using [compose_left_to_right]. *)
module type WITH_KLEISLI_COMPOSITION = sig
  type 'a t
  (** The type held by the [Monad]. *)

  val return : 'a -> 'a t
  (** Create a new ['a t]. *)

  val compose_left_to_right : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t
  (** Composing monadic functions using Kleisli Arrow (from left to right). *)
end

(** {1 Structure anatomy} *)

(** Basis operations. *)
module type CORE = sig
  include WITH_BIND
  (** @closed *)

  include WITH_MAP_AND_JOIN with type 'a t := 'a t
  (** @closed *)

  include WITH_KLEISLI_COMPOSITION with type 'a t := 'a t
  (** @closed *)
end

(** Additional operations. *)
module type OPERATION = sig
  type 'a t
  (** The type held by the [Monad]. *)

  val compose_right_to_left : ('b -> 'c t) -> ('a -> 'b t) -> 'a -> 'c t
  (** Composing monadic functions using Kleisli Arrow (from right to left). *)

  val lift : ('a -> 'b) -> 'a t -> 'b t
  (** Mapping over from ['a] to ['b] over ['a t] to ['b t]. *)

  val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** Mapping over from ['a] and ['b] to ['c] over ['a t] and ['b t] to ['c t]. *)

  val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  (** Mapping over from ['a] and ['b] and ['c] to ['d] over ['a t] and ['b t]
      and ['c t] to ['d t]. *)

  include Functor.OPERATION with type 'a t := 'a t
  (** @closed *)
end

(** Syntax extensions. *)
module type SYNTAX = sig
  type 'a t
  (** The type held by the [Monad]. *)

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  (** Syntaxic shortcuts for flipped version of {!val:CORE.bind}:

      [let* x = e in f] is equals to [bind (fun x -> f) e]. *)

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  (** Syntaxic shortcuts for flipped version of {!val:CORE.map}:

      [let+ x = e in f] is equals to [map (fun x -> f) e]. *)
end

(** Infix operators. *)
module type INFIX = sig
  type 'a t
  (** The type held by the [Monad]. *)

  val ( =|< ) : ('a -> 'b) -> 'a t -> 'b t
  (** Infix version of {!val:CORE.map}. *)

  val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
  (** Infix flipped version of {!val:CORE.map}. *)

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  (** Infix flipped version of {!val:CORE.bind}. *)

  val ( =<< ) : ('a -> 'b t) -> 'a t -> 'b t
  (** Infix version of {!val:CORE.bind}. *)

  val ( >=> ) : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t
  (** Infix version of {!val:CORE.compose_left_to_right}. *)

  val ( <=< ) : ('b -> 'c t) -> ('a -> 'b t) -> 'a -> 'c t
  (** Infix version of {!val:OPERATION.compose_right_to_left}. *)

  val ( >> ) : unit t -> 'b t -> 'b t
  (** Sequentially compose two actions, discarding any value produced by the
      first. *)

  val ( << ) : 'a t -> unit t -> 'a t
  (** Sequentially compose two actions, discarding any value produced by the
      second. *)

  include Functor.INFIX with type 'a t := 'a t
  (** @closed *)
end

(** {1 Complete API} *)

(** The complete interface of a [Monad]. *)
module type API = sig
  (** {1 Core functions}

      Set of fundamental functions in the description of a [Monad]. *)

  include CORE
  (** @closed *)

  (** {1 Additional functions}

      Additional functions, derived from fundamental functions. *)

  include OPERATION with type 'a t := 'a t
  (** @closed *)

  (** {1 Syntax} *)

  module Syntax : SYNTAX with type 'a t := 'a t

  (** {2 Syntax inclusion} *)

  include module type of Syntax
  (** @closed *)

  (** {1 Infix operators} *)

  module Infix : INFIX with type 'a t := 'a t

  (** {2 Infix operators inclusion} *)

  include module type of Infix
  (** @closed *)
end

(** {1 Additional references}

    - {{:http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad.html}
      Haskell's documentation of a Monad}
    - {{:http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf}
      Monad for functional programming}
    - {{:https://person.dibris.unige.it/moggi-eugenio/ftp/ic91.pdf} Notions of
      computations and monads}
    - {{:https://wiki.haskell.org/All_About_Monads} All About Monads} *)
