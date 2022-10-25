(** A [Selective] (applicative functor) allows to declare effects statically and
    select which execute dynamically. It is an algebraic structure between
    {!module:Applicative} and {!module:Monad}. A [Selective] is also an
    {!module:Applicative}. *)

(** {2 Laws}

    To have a predictable behaviour, the instance of [Selective] must obey some
    laws.

    + [x <*? pure id = Either.case id id <$> x]
    + [pure x <*? (y *> z) = (pure x <*? y) *> (pure x <*? z)]
    + [x <*? (y <*? z) = (f <$> x) <*? (g <$> y) <*? (h <$> z)]
    + [f <$> select x y) = (select (Bifunctor.map_snd f <$> x) (((%) f) <$> y)]
    + [(select (Bifunctor.map_fst f <$> x) y) = (select x ((%>) f) <$> y))]
    + [(select x (f <$> y)) = (select (Bifunctor.map_fst (flip f) <$> x) ((|>) \
     <$> y))]
    + [(x <*? pure y) = (Either.case y id <$> x)]
    + [(pure (Right x) <*? y) = pure x]
    + [(pure (Left x) <*? y) = ((|>) x) <$> y]

    {3 Laws for Rigid Selectives}

    A [selective] is [Rigid] if [apply] can be defined in term of [select]

    + [f <*> g = apply f g]
    + [(x *> (y <*? z)) = ((x *> y) <*? z)] *)

(** {1 Minimal definition} *)

(** Minimal definition using [select] without {!module:Applicative}
    requirements. *)
module type WITH_SELECT = sig
  type 'a t
  (** The type held by the [Selective]. *)

  val select : ('a, 'b) Either.t t -> ('a -> 'b) t -> 'b t
  (** [select e f] apply [f] if [e] is [Left]. It allow to skip effect using
      [Right]. *)
end

(** Minimal definition using [branch] without {!module:Applicative}
    requirements. *)
module type WITH_BRANCH = sig
  type 'a t
  (** The type held by the [Selective]. *)

  val branch : ('a, 'b) Either.t t -> ('a -> 'c) t -> ('b -> 'c) t -> 'c t
  (** [branch] is like [select]. It chooses between two effects. *)
end

(** Standard requirement including [pure] and [select]. *)
module type WITH_PURE_AND_SELECT = sig
  include WITH_SELECT
  (** @inline *)

  val pure : 'a -> 'a t
  (** Create a new ['a t]. *)
end

(** Standard requirement including [pure] and [branch]. *)
module type WITH_PURE_AND_BRANCH = sig
  include WITH_BRANCH
  (** @inline *)

  val pure : 'a -> 'a t
  (** Create a new ['a t]. *)
end

(** {1 Structure anatomy} *)

(** Basis operation. *)
module type CORE = sig
  include WITH_SELECT
  (** @inline *)

  include WITH_BRANCH with type 'a t := 'a t
  (** @inline *)

  include Applicative.CORE with type 'a t := 'a t
  (** @inline *)
end

(** Additional operations. *)
module type OPERATION = sig
  type 'a t
  (** The type held by the [Selective]. *)

  include Applicative.OPERATION with type 'a t := 'a t
  (** @inline *)

  val if_ : bool t -> 'a t -> 'a t -> 'a t
  (** Same of [branch] but using a [Boolean] as disjunction. *)

  val bind_bool : bool t -> (bool -> 'a t) -> 'a t
  (** {!module:Monad} [bind] specialized for Boolean. *)

  val when_ : bool t -> unit t -> unit t
  (** Conditionally perform an effect. *)

  val exists : ('a -> bool t) -> 'a list -> bool t
  (** Selective version of [List.exists]. *)

  val for_all : ('a -> bool t) -> 'a list -> bool t
  (** Selective version of [List.for_all]. *)

  val or_ : bool t -> bool t -> bool t
  (** Or combinator. *)

  val and_ : bool t -> bool t -> bool t
  (** And combinator. *)

  val while_ : bool t -> unit t
  (** Keep checking an effectful condition while it holds. *)
end

(** Syntax extensions. *)
module type SYNTAX = sig
  include Applicative.SYNTAX
  (** @inline *)
end

(** Infix operators. *)
module type INFIX = sig
  type 'a t
  (** The type held by the [Selective]. *)

  include Applicative.INFIX with type 'a t := 'a t
  (** @inline *)

  val ( <*? ) : ('a, 'b) Either.t t -> ('a -> 'b) t -> 'b t
  (** Infix version of {!val:CORE.select}. *)

  val ( <||> ) : bool t -> bool t -> bool t
  (** Infix version of {!val:CORE.or_}. *)

  val ( <&&> ) : bool t -> bool t -> bool t
  (** Infix version of {!val:CORE.and_}. *)
end

(** {1 Complete API} *)

(** The complete interface of a [Selective]. *)
module type API = sig
  (** {1 Type} *)

  type 'a t
  (** The type held by the [Selective]. *)

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

(** {1 Additional references}

    - {{:http://hackage.haskell.org/package/selective} Haskell's documentation
      of a Selective Application Functor}
    - {{:https://www.staff.ncl.ac.uk/andrey.mokhov/selective-functors.pdf}
      Selective Applicative Functors} *)
