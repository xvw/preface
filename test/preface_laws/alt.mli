(** Test cases for [Alt] *)

module Semigroup_cases
    (F : Preface_specs.ALT)
    (A : Preface_qcheck.Model.T1 with type 'a t = 'a F.t)
    (T : Preface_qcheck.Sample.PACKAGE) : sig
  val cases : int -> unit Alcotest.test_case list
end
