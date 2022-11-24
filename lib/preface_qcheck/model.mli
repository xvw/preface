(** Describes all the elements required to build a test suite. *)

(** {1 Suit description} *)

module type SUITE = sig
  val tests : count:int -> QCheck2.Test.t list
end

(** {1 Sampling Data} *)

module type T0 = sig
  type t

  val generator : t QCheck2.Gen.t
  val observable : t QCheck2.Observable.t
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end

(** {1 Types of arity 0}

    Describes the set of prerequisites for building generators on arity-free
    types (notably the monoid hierarchy). *)

module type COVARIANT_0 = sig
  type t

  val generator : t QCheck2.Gen.t
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end

(** {1 Types of arity 1} *)

module type COVARIANT_1 = sig
  type 'a t

  val generator : 'a QCheck2.Gen.t -> 'a t QCheck2.Gen.t
  val observable : 'a QCheck2.Observable.t -> 'a t QCheck2.Observable.t
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end

module type CONTRAVARIANT_1 = sig
  type 'a t
  type 'a generator
  type 'a input

  val generator : 'a QCheck2.Observable.t -> 'a generator QCheck2.Gen.t
  val input : 'a QCheck2.Gen.t -> 'a input QCheck2.Gen.t
  val lift : 'a generator -> 'a t
  val run_equality : 'a input -> 'a t -> 'a t -> bool
end

(** {1 Types of arity 2} *)

module type COVARIANT_2 = sig
  type ('a, 'b) t

  val generator :
    'a QCheck2.Gen.t -> 'b QCheck2.Gen.t -> ('a, 'b) t QCheck2.Gen.t

  val observable :
       'a QCheck2.Observable.t
    -> 'b QCheck2.Observable.t
    -> ('a, 'b) t QCheck2.Observable.t

  val pp :
       (Format.formatter -> 'a -> unit)
    -> (Format.formatter -> 'b -> unit)
    -> Format.formatter
    -> ('a, 'b) t
    -> unit

  val equal :
    ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
end

module type PROFUNCTORIAL = sig
  type ('a, 'b) t
  type ('a, 'b) generator
  type 'a input
  type 'a output

  val generator :
       'a QCheck2.Observable.t
    -> 'b QCheck2.Gen.t
    -> ('a, 'b) generator QCheck2.Gen.t

  val input : 'a QCheck2.Gen.t -> 'a input QCheck2.Gen.t
  val lift : ('a, 'b) generator -> ('a, 'b) t
  val equal : ('b -> 'b -> bool) -> 'b output -> 'b output -> bool
  val run : ('a, 'b) t -> 'a input -> 'b output
  val run_functional_output : ('a -> 'b) output -> 'a -> 'b output
  val map_input : ('a -> 'b) -> 'a input -> 'b input

  val run_equality :
       'a input
    -> ('b output -> 'b output -> bool)
    -> ('a, 'b) t
    -> ('a, 'b) t
    -> bool
end
