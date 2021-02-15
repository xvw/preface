(** Test cases for [Alternatives] *)

module Cases_for_monoidal
    (F : Preface_specs.ALTERNATIVE)
    (A : Preface_qcheck.Model.T1 with type 'a t = 'a F.t)
    (T : Preface_qcheck.Sample.PACKAGE) : sig
  val cases : int -> unit Alcotest.test_case list
end

module Cases_for_apply_right_distribituvity
    (F : Preface_specs.ALTERNATIVE)
    (A : Preface_qcheck.Model.T1 with type 'a t = 'a F.t)
    (T : Preface_qcheck.Sample.PACKAGE) : sig
  val cases : int -> unit Alcotest.test_case list
end

module Cases_for_right_absorption
    (F : Preface_specs.ALTERNATIVE)
    (A : Preface_qcheck.Model.T1 with type 'a t = 'a F.t)
    (T : Preface_qcheck.Sample.PACKAGE) : sig
  val cases : int -> unit Alcotest.test_case list
end

module Cases
    (F : Preface_specs.ALTERNATIVE)
    (A : Preface_qcheck.Model.T1 with type 'a t = 'a F.t)
    (T : Preface_qcheck.Sample.PACKAGE) : sig
  val cases : int -> unit Alcotest.test_case list
end
