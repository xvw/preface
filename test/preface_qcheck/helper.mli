(** Helper to build test-suite *)

(** {2 Easy access to observables and arbitrary} *)

module type PACKED = sig
  module P : Sample.PACK

  val t1 : P.T1.input Arbitrary.t

  val t1' : P.T1.input QCheck.Observable.t

  val t2 : P.T2.input Arbitrary.t

  val t2' : P.T2.input QCheck.Observable.t

  val t3 : P.T3.input Arbitrary.t

  val t3' : P.T3.input QCheck.Observable.t

  val t4 : P.T4.input Arbitrary.t

  val t4' : P.T4.input QCheck.Observable.t

  val t5 : P.T5.input Arbitrary.t

  val t5' : P.T5.input QCheck.Observable.t

  val t6 : P.T6.input Arbitrary.t

  val t6' : P.T6.input QCheck.Observable.t
end

(** {2 Build an helper for a test-suite} *)

module Make_for_t (R : Requirement.INPUT_T) : sig
  val over : R.t Arbitrary.t
end

module Make_for_t1 (R : Requirement.INPUT_T1) (P : Sample.PACK) : sig
  val over : 'a Arbitrary.t -> 'a R.t Arbitrary.t

  include PACKED
end

module Make_for_t2 (R : Requirement.INPUT_T2) (P : Sample.PACK) : sig
  val over : 'a Arbitrary.t -> 'b Arbitrary.t -> ('a, 'b) R.t Arbitrary.t

  include PACKED
end
