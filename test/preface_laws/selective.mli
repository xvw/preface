(** Test cases for [Selectives] *)

module Cases
    (F : Preface_specs.SELECTIVE)
    (A : Preface_qcheck.Model.T1 with type 'a t = 'a F.t)
    (T : Preface_qcheck.Sample.PACKAGE) : sig
  val cases : int -> unit Alcotest.test_case list
end

(** {1 Special case for Rigid Selective Functors}

    Standard cases are also includes in the test suite. *)

module Rigid_cases
    (F : Preface_specs.SELECTIVE)
    (A : Preface_qcheck.Model.T1 with type 'a t = 'a F.t)
    (T : Preface_qcheck.Sample.PACKAGE) : sig
  val cases : int -> unit Alcotest.test_case list
end
