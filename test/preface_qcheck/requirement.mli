(** Requirements for test-suite *)

(** {2 Hooking}

    A [Hook] is applied before test equalities. In order to deal with [Stream]
    and [Continuation]. *)

module type HOOK = sig
  type 'a t

  val apply : 'a t -> 'b
end

(** {2 Define a test suite} *)

module type INPUT = sig
  val name : string

  val size : int
end

module type INPUT_T = sig
  include INPUT

  type t

  val arbitrary : t Arbitrary.t
end

module type INPUT_T1 = sig
  include INPUT

  type 'a t

  val arbitrary : 'a Arbitrary.t -> 'a t Arbitrary.t
end

module type INPUT_T2 = sig
  include INPUT

  type ('a, 'b) t

  val arbitrary : 'a Arbitrary.t -> 'b Arbitrary.t -> ('a, 'b) t Arbitrary.t
end

module type OUTPUT = sig
  val cases : (string * unit Alcotest.test_case list) list
end
