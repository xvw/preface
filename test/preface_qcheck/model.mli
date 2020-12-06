module type T0 = sig
  type t

  val arbitrary : t Arbitrary.t

  val observable : t Observable.t

  val equal : t -> t -> bool
end

module type T1 = sig
  type 'a t

  val arbitrary : 'a QCheck.arbitrary -> 'a t QCheck.arbitrary

  val observable : 'a QCheck.Observable.t -> 'a t Observable.t

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end

module type T2 = sig
  type ('a, 'b) t

  val arbitrary :
    'a QCheck.arbitrary -> 'b QCheck.arbitrary -> ('a, 'b) t QCheck.arbitrary

  val observable :
    'a QCheck.Observable.t -> 'b QCheck.Observable.t -> ('a, 'b) t Observable.t

  val equal :
    ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
end
