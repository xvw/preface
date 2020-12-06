(** Test cases for [Monoid] *)

module Cases
    (F : Preface_specs.MONOID)
    (A : Preface_qcheck.Model.T0 with type t = F.t) : sig
  val cases : int -> unit Alcotest.test_case list
end
