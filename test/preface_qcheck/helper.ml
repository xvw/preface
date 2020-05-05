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

module Package (P : Sample.PACK) : PACKED = struct
  module P = P

  let t1 = P.T1.arbitrary

  let t2 = P.T2.arbitrary

  let t3 = P.T3.arbitrary

  let t4 = P.T4.arbitrary

  let t5 = P.T5.arbitrary

  let t6 = P.T6.arbitrary

  let t1' = P.T1.observable

  let t2' = P.T2.observable

  let t3' = P.T3.observable

  let t4' = P.T4.observable

  let t5' = P.T5.observable

  let t6' = P.T6.observable
end

module Make_for_t (R : Requirement.INPUT_T) = struct
  let over = R.arbitrary
end

module Make_for_t1 (R : Requirement.INPUT_T1) (P : Sample.PACK) = struct
  let over x = R.arbitrary x

  include Package (P)
end

module Make_for_t2 (R : Requirement.INPUT_T2) (P : Sample.PACK) = struct
  let over x y = R.arbitrary x y

  include Package (P)
end
