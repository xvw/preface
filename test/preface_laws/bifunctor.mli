(** Test cases for [Bifunctor] *)

module Cases
    (F : Preface_specs.BIFUNCTOR)
    (A : Preface_qcheck.Model.T2 with type ('a, 'b) t = ('a, 'b) F.t)
    (T : Preface_qcheck.Sample.PACKAGE) : sig
  val cases : int -> unit Alcotest.test_case list
end
